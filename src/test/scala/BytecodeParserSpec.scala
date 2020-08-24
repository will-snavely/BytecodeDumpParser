import model._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import parser._

import scala.util.{Failure, Success}

class BytecodeParserSpec extends AnyFlatSpec with should.Matchers {
  "A parser" should "compute opcode base-names" in {
    Map(
      "iload" -> "iload",
      "fast_iload" -> "iload",
      "nofast_iload" -> "iload"
    ).foreachEntry((input, expected) =>
      BytecodeDumpParser.baseName(input) should be(expected)
    )
  }

  it should "parse returns correctly" in {
    Map(
      "[0] 1 0 return" ->
        Success(InstructionLine(instruction = Return, counter = 1, offset = 0)),
      "[0] 2 1 freturn" ->
        Success(InstructionLine(instruction = FReturn, counter = 2, offset = 1)),
      "[0] 3 2 dreturn" ->
        Success(InstructionLine(instruction = DReturn, counter = 3, offset = 2)),
      "[0] 4 3 lreturn" ->
        Success(InstructionLine(instruction = LReturn, counter = 4, offset = 3)),
      "[0] 5 4 areturn" ->
        Success(InstructionLine(instruction = AReturn, counter = 5, offset = 4)),
      "[0] 6 5 ireturn" ->
        Success(InstructionLine(instruction = IReturn, counter = 6, offset = 5)),
    ).foreachEntry((input, expected) =>
      BytecodeDumpParser.parseTraceLine(input) should be(expected)
    )
  }

  it should "parse non-dynamic invokes correctly" in {
    Map(
      "[0] 1 2 invokestatic 3 <class.func()Lret;>" ->
        Success(InstructionLine(
          instruction = InvokeStatic(ConstantPoolIndex(
            Some(3),
            Some(MethodHint("class", "func", "()Lret;")))),
          counter = 1,
          offset = 2)),
      "[0] 2 3 invokespecial 4 <class.func()Lret;>" ->
        Success(InstructionLine(
          instruction = InvokeSpecial(ConstantPoolIndex(
            Some(4),
            Some(MethodHint("class", "func", "()Lret;")))),
          counter = 2,
          offset = 3)),
      "[0] 3 4 invokevirtual 5 <class.func()Lret;>" ->
        Success(InstructionLine(
          instruction = InvokeVirtual(ConstantPoolIndex(
            Some(5),
            Some(MethodHint("class", "func", "()Lret;")))),
          counter = 3,
          offset = 4)),
      "[0] 4 5 invokeinterface 6 <class.func()Lret;>" ->
        Success(InstructionLine(
          instruction = InvokeInterface(ConstantPoolIndex(
            Some(6),
            Some(MethodHint("class", "func", "()Lret;")))),
          counter = 4,
          offset = 5)),
      "[0] 5 6 invokehandle 7 <class.func()Lret;>" ->
        Success(InstructionLine(
          instruction = InvokeHandle(ConstantPoolIndex(
            Some(7),
            Some(MethodHint("class", "func", "()Lret;")))),
          counter = 5,
          offset = 6)),
    ).foreachEntry((input, expected) =>
      BytecodeDumpParser.parseTraceLine(input) should be(expected)
    )
  }

  it should "parse dynamic invokes correctly" in {
    Map(
      "[0] 1 2 invokedynamic bsm=3 4 <class.func()Lret;>" ->
        Success(InstructionLine(
          instruction = InvokeDynamic(Some(3), ConstantPoolIndex(
            Some(4),
            Some(MethodHint("class", "func", "()Lret;")))),
          counter = 1,
          offset = 2)),
      "[0] 2 3 invokedynamic 5 <class.func()Lret;>" ->
        Success(InstructionLine(
          instruction = InvokeDynamic(None, ConstantPoolIndex(
            Some(5),
            Some(MethodHint("class", "func", "()Lret;")))),
          counter = 2,
          offset = 3)),
    ).foreachEntry((input, expected) =>
      BytecodeDumpParser.parseTraceLine(input) should be(expected)
    )
  }

  it should "fail on malformed input lines" in {
    val badLine = "some random input"
    BytecodeDumpParser.parseTraceLine(badLine) should be(
      Failure(MalformedLine(badLine, None))
    )
  }

  it should "fail on empty input lines" in {
    val badLine = ""
    BytecodeDumpParser.parseTraceLine(badLine) should be(
      Failure(MalformedLine(badLine, Some(EmptyLine)))
    )
  }
}
