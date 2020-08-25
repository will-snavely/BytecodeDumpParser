import java.io.{BufferedInputStream, FileInputStream, FileReader, PrintWriter}
import java.util.zip.GZIPInputStream

import model.{CallData, CallTree, FlatNode, NaryTree}
import org.json4s._
import org.json4s.native.Serialization.read

import scala.collection.mutable

class VariableSum {
  var constantPart: Int = 0
  val variables: mutable.Map[String, Int] = new mutable.HashMap().withDefaultValue(0)
}

class AccData(val signature: String) {
  val totalInstructions: VariableSum = new VariableSum()
  val totalCalls: VariableSum = new VariableSum()
}

object Analyze {
  implicit val jsonDefaultFormats: DefaultFormats.type = DefaultFormats

  val denyList = List(
    (s: String) => s.startsWith("java.lang.ClassLoader"),
    (s: String) => s.startsWith("jdk.internal"),
  )

  def indent(level: Int, out: PrintWriter): Unit = {
    var i = level
    while (i > 0) {
      out.print("  ")
      i -= 1
    }
  }

  def accumulate(tree: NaryTree[CallData]): NaryTree[AccData] = {
    val newTree = new NaryTree(new AccData(tree.data.signature))
    newTree.data.totalInstructions.constantPart = tree.data.instructionCount
    tree.children.foreach(child => {
      val newChild = accumulate(child)
      newTree.children.append(newChild)
      if (child.data.isStub) {
        val sig = child.data.signature
        val instMap = newTree.data.totalInstructions.variables
        val callMap = newTree.data.totalCalls.variables
        instMap.update(sig, instMap(sig) + 1)
        callMap.update(sig, callMap(sig) + 1)
      } else {
        newTree.data.totalInstructions.constantPart += newChild.data.totalInstructions.constantPart
        newTree.data.totalCalls.constantPart += (1 + newChild.data.totalCalls.constantPart)
      }
    })
    newTree
  }

  def gis(s: String) = new GZIPInputStream(new BufferedInputStream(new FileInputStream(s)))

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println("Usage: Analyze <tree.json>")
      System.exit(-1)
    }

    val reader = new FileReader(args(0))
    val nodeList = read[List[FlatNode]](reader)
    val tree = CallTree.fromFlatList(nodeList)
    val treeFile = new PrintWriter("tree.txt")
    val accFile = new PrintWriter("acc.txt")

    try {
      tree.visitAtDepth((data, depth) => {
        indent(depth, treeFile)
        treeFile.println(data.signature)
      })
      accumulate(tree).visitAtDepth((data, depth) => {
        indent(depth, accFile)
        accFile.println(data.signature)
        indent(depth + 1, accFile)
        accFile.print(data.totalCalls.constantPart)
        data.totalCalls.variables.foreach(item => {
          accFile.print(" + ")
          accFile.print("(%s * %s)", item._1, item._2)
        })
        accFile.println()
      })
    } finally {
      treeFile.close()
      accFile.close()
    }
  }
}
