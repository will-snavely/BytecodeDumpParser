import java.io.{BufferedInputStream, FileInputStream, FileReader, PrintWriter}
import java.util.zip.GZIPInputStream

import model.{CallData, CallTree, FlatNode, NaryTree}
import org.json4s._
import org.json4s.native.Serialization.read
import org.json4s.native.Serialization.write

import scala.collection.mutable

class AccData
(
  val signature: String,
  var totalInstructions: Int = 0,
  var totalSeenCalls: Int = 0,
  val totalStubCalls: mutable.Map[String, Int] = new mutable.HashMap().withDefaultValue(0),
  val immediateCalls: mutable.Map[String, Int] = new mutable.HashMap().withDefaultValue(0)
)

object Analyze {
  implicit val jsonDefaultFormats: DefaultFormats.type = DefaultFormats

  val denyList = List(
    (s: String) => s.startsWith("java.lang.ClassLoader"),
    (s: String) => s.startsWith("jdk.internal")
  )

  def indent(level: Int, out: PrintWriter): Unit = {
    var i = level
    while (i > 0) {
      out.print("  ")
      i -= 1
    }
  }

  def getName(signature: String): String = {
    signature
      .split("\\.").last
      .split("\\(").head
      .split("\\$").head
  }

  def accumulate(tree: NaryTree[CallData]): NaryTree[AccData] = {
    val newTree = new NaryTree(new AccData(tree.data.signature))
    newTree.data.totalInstructions = tree.data.instructionCount
    tree.children.foreach(child => {
      val newChild = accumulate(child)
      val stubMap = newTree.data.totalStubCalls
      val immCallMap = newTree.data.immediateCalls
      val childSig = getName(child.data.signature)

      immCallMap.update(childSig, immCallMap(childSig) + 1)
      if (child.data.isStub) {
        stubMap.update(childSig, stubMap(childSig) + 1)
      } else {
        newTree.children.append(newChild)
        newTree.data.totalInstructions += newChild.data.totalInstructions
        newTree.data.totalSeenCalls += (1 + newChild.data.totalSeenCalls)
        newChild.data.totalStubCalls.foreachEntry((sig, count) => {
          stubMap.update(sig, stubMap(sig) + count)
        })
      }
    })
    newTree
  }

  def gis(s: String) = new GZIPInputStream(new BufferedInputStream(new FileInputStream(s)))

  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      System.err.println("Usage: Analyze <tree.json> <function_regex>")
      System.exit(-1)
    }

    implicit val jsonDefaultFormats: DefaultFormats = DefaultFormats
    val reader = new FileReader(args(0))
    val functionMatcher = args(1)
    val nodeList = read[List[FlatNode]](reader)
    val tree = CallTree.fromFlatList(nodeList)
    val treeFile = new PrintWriter("tree.txt")
    val resultsFile = new PrintWriter("result.json")

    try {
      tree.visitAtDepth((data, depth) => {
        indent(depth, treeFile)
        treeFile.println("%s, %s".format(data.signature, data.isStub))
      })
      accumulate(tree)
        .findAll(acc => acc.signature.matches(functionMatcher))
        .map(results => {
          write(results.map(_.data), resultsFile)
        })
        .recover({
          case _: NoSuchElementException =>
            System.err.println("No function found matching: " + functionMatcher)
        })
    } finally {
      treeFile.close()
      resultsFile.close()
    }
  }
}
