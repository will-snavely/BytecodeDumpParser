import java.io.{BufferedInputStream, FileInputStream, FileWriter}
import java.nio.charset.CodingErrorAction

import model._
import org.json4s._
import org.json4s.native.Serialization.write
import parser._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.{Codec, Source}

object BuildTree {
  def buildBlocks(trace: List[TraceLine]): IndexedSeq[Block] = {
    val result = ListBuffer[Block]()
    var cur = trace
    var curBlock: Option[Block] = None
    while (cur.nonEmpty) {
      cur.head match {
        case InstructionLine(instruction, _, _) =>
          curBlock
            .map(block => block.instructions.append(instruction))
            .orElse(throw new Exception("Failed to associate instruction with method"))
        case method: MethodEntryLine =>
          curBlock.map(block => result.append(block))
          curBlock = Some(new Block(method, ListBuffer()))
        case _ =>
          ;
      }
      cur = cur.tail
    }
    result.toIndexedSeq
  }

  def sliceBlocksFrom(blocks: IndexedSeq[Block], from: String): IndexedSeq[Block] =
    blocks.indexWhere(_.method.signature == from) match {
      case -1 => throw new IllegalArgumentException("Signature '%s' not found.".format(from))
      case index => blocks.slice(index, blocks.length)
    }

  def buildCallTree(blocks: IndexedSeq[Block]): NaryTree[CallData] = {
    var blockHead = blocks
    val stack = new mutable.Stack[NaryTree[CallData]]()
    val root = new NaryTree(new CallData("<root>", false))

    stack.push(root)
    var prevBlock: Option[Block] = None

    while (blockHead.nonEmpty && stack.nonEmpty) {
      val head = blockHead.head
      val method = head.method
      val parent = stack.head

      if (head.method.signature != parent.data.signature) {
        val child = new NaryTree(new CallData(method.signature, false))
        parent.children.prepend(child)
        stack.push(child)
      } else {
        parent.data.instructionCount += head.instructions.length
      }

      head.instructions.slice(0, head.instructions.length - 1).foreach({
        case invoke: Invoke =>
          invoke.operand.hint.map({
            case m: MethodHint =>
              parent.children.prepend(
                new NaryTree(
                  new CallData("%s%s".format(m.name, m.descriptor), true)))
            case _ => ;
          })
          parent.children.prepend(
            new NaryTree(new CallData(method.signature, true)))
        case _ => ;
      })

      head.instructions.last match {
        case _: ReturnInst =>
          if (stack.head.data.signature != head.method.signature) {
            System.err.println("WARNING: Pop doesn't match push")
          }
          stack.pop()
        case _ => ;
      }

      prevBlock = Some(head)
      blockHead = blockHead.tail
    }

    println("Remaining calls")
    stack.foreach(item => println(item.data.signature))

    root
  }

  def inputStream(s: String) = new BufferedInputStream(new FileInputStream(s))

  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      System.err.println("Usage: Parse <dump_path> <out>")
      System.exit(-1)
    }

    implicit val jsonDefaultFormats: DefaultFormats = DefaultFormats
    implicit val codec: Codec = Codec("UTF-8")
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

    val source = Source.fromInputStream(inputStream(args(0)))
    val treeOutput = new FileWriter(args(1))
    try {
      val trace = BytecodeDumpParser.parseFileStream(source)
        .filter(_.isSuccess)
        .map(_.get)
        .toList
      val blocks = buildBlocks(trace)
      val tree = buildCallTree(blocks)
      write(CallTree.flatten(tree), treeOutput)
    } finally {
      source.close()
      treeOutput.close()
    }
  }
}
