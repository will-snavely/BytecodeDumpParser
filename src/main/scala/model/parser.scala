package model

trait ResolutionHint
case class UnresolvedHint(at: String) extends ResolutionHint
case class MethodHint(className: String, name: String, descriptor: String) extends ResolutionHint
case class FieldHint(className: String, fieldName: String, descriptor: String) extends ResolutionHint
case class TypeHint(name: String) extends ResolutionHint

trait Operand
case class ConstantPoolIndex(index: Option[Int], hint: Option[ResolutionHint]) extends Operand
case class LocalVariableIndex(index: Int) extends Operand
case class BranchOffset(offset: Int) extends Operand
case class ConstantValue(value: Option[String], hint: Option[ResolutionHint]) extends Operand
case class Dimension(value: Int) extends Operand
case class Increment(value: Int) extends Operand

trait ShiftKind
case object Arithmetic extends ShiftKind
case object Logical extends ShiftKind

trait Instruction
trait Nullary extends Instruction
trait Unary extends Instruction { val operand: Operand }

trait Sized
trait ByteSized extends Sized
trait ShortSized extends Sized
trait IntSized extends Sized
trait DoubleSized extends Sized
trait FloatSized extends Sized
trait LongSized extends Sized
trait AddressSized extends Sized

trait StackNumeric extends Nullary with Sized

trait Div extends StackNumeric
case object IDiv extends Div with IntSized
case object FDiv extends Div with FloatSized
case object DDiv extends Div with DoubleSized
case object LDiv extends Div with LongSized

trait Rem extends StackNumeric
case object IRem extends Rem with IntSized
case object FRem extends Rem with FloatSized
case object DRem extends Rem with DoubleSized
case object LRem extends Rem with LongSized

trait Add extends StackNumeric
case object IAdd extends Add with IntSized
case object FAdd extends Add with FloatSized
case object DAdd extends Add with DoubleSized
case object LAdd extends Add with LongSized

trait Sub extends StackNumeric
case object ISub extends Sub with IntSized
case object FSub extends Sub with FloatSized
case object DSub extends Sub with DoubleSized
case object LSub extends Sub with LongSized

trait Mul extends StackNumeric
case object IMul extends Mul with IntSized
case object FMul extends Mul with FloatSized
case object DMul extends Mul with DoubleSized
case object LMul extends Mul with LongSized

trait Shift extends StackNumeric
trait Arithmetic
trait Logical
trait ShiftLeft extends Shift
case object IShl extends ShiftLeft with IntSized with Arithmetic
case object IUShl extends ShiftLeft with IntSized with Logical
case object LShl extends ShiftLeft with LongSized with Arithmetic
case object LUShl extends ShiftLeft with LongSized with Logical

trait ShiftRight extends Shift
case object IShr extends ShiftRight with IntSized with Arithmetic
case object IUShr extends ShiftRight with IntSized with Logical
case object LShr extends ShiftRight with LongSized with Arithmetic
case object LUShr extends ShiftRight with LongSized with Logical

case class New(operand: ConstantPoolIndex) extends Unary
case class CheckCast(operand: ConstantPoolIndex) extends Unary
case class InstanceOf(operand: ConstantPoolIndex) extends Unary

trait Invoke extends Unary { val operand: ConstantPoolIndex }
case class InvokeStatic(operand: ConstantPoolIndex) extends Invoke
case class InvokeVirtual(operand: ConstantPoolIndex) extends Invoke
case class InvokeSpecial(operand: ConstantPoolIndex) extends Invoke
case class InvokeInterface(operand: ConstantPoolIndex) extends Invoke
case class InvokeHandle(operand: ConstantPoolIndex) extends Invoke
case class InvokeDynamic(bsm: Option[Int], operand: ConstantPoolIndex) extends Invoke

trait ReturnInst extends Nullary
case object Return extends ReturnInst
case object IReturn extends ReturnInst with IntSized
case object LReturn extends ReturnInst with IntSized
case object FReturn extends ReturnInst with FloatSized
case object DReturn extends ReturnInst with DoubleSized
case object AReturn extends ReturnInst with AddressSized

case class Other(code: String, args: String) extends Instruction

case class MethodEntry(invokeType: String, ret: String, signature: String)

trait TraceLine
case class MethodEntryLine(invokeType: String, ret: String, signature: String) extends TraceLine
case class InstructionLine(instruction: Instruction, counter: Int, offset: Int) extends TraceLine
case class SummaryLine(bytecodeCount: Int, duration: Double) extends TraceLine
case class CounterLine(bytecodeCount: Int) extends TraceLine

trait RawLine
case class RawMethod(invokeType: String, ret: String, signature: String) extends RawLine
case class RawInstructionLine(counter: String, offset: String, instruction: String, text: String) extends RawLine
case class RawSummaryLine(bytecodeCount: String, duration: String, text: String) extends RawLine
case class RawCounterLine(bytecodeCount: String, text: String) extends RawLine
case class RawInstruction(opcode: String, operands: String)