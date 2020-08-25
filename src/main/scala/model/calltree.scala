package model

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class NaryTree[A](val data: A) {
  val children: ListBuffer[NaryTree[A]] = ListBuffer()

  def visitAtDepth(f: (A, Int) => Unit): Unit = {
    val stack = new mutable.Stack[(NaryTree[A], Int)]()
    stack.push((this, 0))
    while (stack.nonEmpty) {
      stack.pop() match {
        case (tree, level) =>
          f(tree.data, level)
          tree.children.foreach(child => {
            stack.push((child, level + 1))
          })
      }
    }
  }

  def find(p: A => Boolean): Option[NaryTree[A]] = {
    val stack = new mutable.Stack[NaryTree[A]]()
    stack.push(this)
    while (stack.nonEmpty) {
      val tree = stack.pop()
      if (p(tree.data)) {
        return Some(tree)
      }
      tree.children.foreach(child => {
        stack.push(child)
      })
    }
    None
  }
}

case class FlatNode(
                     i: Int, // Unique Node ID
                     s: String, // Signature
                     c: Int, // Instruction count
                     b: Int, // isStub
                     p: Int // Parent ID
                   )

object CallTree {
  def toInt(b: Boolean): Int = {
    case true => 1
    case false => 0
  }

  def flatten(tree: NaryTree[CallData]): List[FlatNode] = {
    val queue = mutable.Queue[(NaryTree[CallData], Int)]()
    val buffer = ListBuffer[FlatNode]()
    var id = 0
    queue.enqueue((tree, -1))
    while (queue.nonEmpty) {
      val curId = id
      id += 1
      queue.dequeue() match {
        case (node, parent) =>
          buffer.append(
            FlatNode(
              curId,
              node.data.signature,
              node.data.instructionCount,
              toInt(node.data.isStub), parent))
          node.children.foreach(child => queue.enqueue((child, curId)))
      }
    }
    buffer.toList
  }

  def fromFlatList(nodes: List[FlatNode]): NaryTree[CallData] = {
    if (nodes.isEmpty) {
      new NaryTree(new CallData("<root>", false))
    }
    val nodeMap = mutable.HashMap[Int, NaryTree[CallData]]()
    nodes.foreach(node => {
      val tree = new NaryTree(new CallData(node.s, node.b == 1, node.c))
      nodeMap.get(node.p).map(_.children.append(tree))
      nodeMap.put(node.i, tree)
    })
    nodeMap.getOrElse(0, throw new Exception("Can't find expected root element."))
  }
}

class CallData(
                val signature: String,
                val isStub: Boolean,
                var instructionCount: Int = 0
              )

class CompressedBlock(val method: String, val opcodes: IndexedSeq[Int]) {}

class CompressedBlockTrace(val key: Map[Int, String], val blocks: IndexedSeq[CompressedBlock]) {
  def getInstructionNames(block: CompressedBlock): IndexedSeq[String] = {
    block.opcodes.map(key(_))
  }
}

class Block(val method: MethodEntryLine, val instructions: ListBuffer[Instruction])
