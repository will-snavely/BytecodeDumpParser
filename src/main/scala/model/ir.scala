package model

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class FlatNode(i: Int, s: String, c: Int, p: Int)

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

class CallData(val signature: String, var instructionCount: Int = 0)

class CompressedBlock(val method: String, val opcodes: IndexedSeq[Int]) {}

class CompressedBlockTrace(val key: Map[Int, String], val blocks: IndexedSeq[CompressedBlock]) {
  def getInstructionNames(block: CompressedBlock): IndexedSeq[String] = {
    block.opcodes.map(key(_))
  }
}

class Block(val method: MethodEntryLine, val instructions: ListBuffer[Instruction])
