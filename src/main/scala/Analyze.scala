import java.io.{BufferedInputStream, FileInputStream, FileReader, FileWriter, PrintWriter}
import java.util.zip.GZIPInputStream
import scala.collection.mutable

import org.json4s._
import org.json4s.native.Serialization.read

import model.{CallData, FlatNode, NaryTree}

class AccData
(
  val signature: String,
  var totalInstructions: Int = 0,
  var totalCalls: Int = 0,
  var callsPerFunction: mutable.HashMap[String, Int] = mutable.HashMap()
)

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
    newTree.data.totalInstructions = tree.data.instructionCount
    tree.children.foreach(child => {
      val newChild = accumulate(child)
      newTree.children.append(newChild)
      newTree.data.callsPerFunction.update(
        child.data.signature,
        newTree.data.callsPerFunction.getOrElse(child.data.signature, 0) + 1
      )
      newTree.data.totalInstructions += newChild.data.totalInstructions
      newTree.data.totalCalls += (1 + newChild.data.totalCalls)
      newChild.data.callsPerFunction.foreachEntry((sig, count) => {
        newTree.data.callsPerFunction.update(
          sig,
          newTree.data.callsPerFunction.getOrElse(sig, 0) + count
        )
      })
    })
    newTree
  }

  def fromFlatList(nodes: List[FlatNode]): NaryTree[CallData] = {
    if (nodes.isEmpty) {
      new NaryTree(new CallData("<root>"))
    }
    val nodeMap = mutable.HashMap[Int, NaryTree[CallData]]()
    nodes.foreach(node => {
      val tree = new NaryTree(new CallData(node.s, node.c))
      nodeMap.get(node.p).map(_.children.append(tree))
      nodeMap.put(node.i, tree)
    })
    nodeMap.getOrElse(0, throw new Exception("Can't find expected root element."))
  }

  def gis(s: String) = new GZIPInputStream(new BufferedInputStream(new FileInputStream(s)))

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println("Usage: Analyze <tree.json>")
      System.exit(-1)
    }

    val reader = new FileReader(args(0))
    val nodeList = read[List[FlatNode]](reader)
    val tree = fromFlatList(nodeList)
    val treeFile = new PrintWriter("tree.txt")
    val accFile = new PrintWriter("acc.txt")

    try {
      tree.visitAtDepth((data, depth) => {
        indent(depth, treeFile)
        treeFile.println(data.signature)
      })
      accumulate(tree).visitAtDepth((data, depth) => {
        indent(depth, accFile)
        accFile.println(data.signature, data.totalCalls, data.totalInstructions)
        data.callsPerFunction.foreachEntry((sig, count) => {
          indent(depth, accFile)
          accFile.println("%s: %d".format(sig, count))
        })
      })
    } finally {
      treeFile.close()
      accFile.close()
    }
  }
}
