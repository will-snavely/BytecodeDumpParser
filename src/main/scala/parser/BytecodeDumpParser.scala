package parser

import model._
import scala.io.Source
import scala.util.{Failure, Success, Try}

object BytecodeDumpParser {

  implicit class RegexOps(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  def parseRawLine(text: String): Try[RawLine] = {
    text match {
      case r"(\d+)${count} bytecodes executed in (.+)${time}s \(.*\)" =>
        Success(RawSummaryLine(count, time, text))
      case r"\[BytecodeCounter::counter_value = (\d*)${count}\]" =>
        Success(RawCounterLine(count, text))
      case r"\[\d*\]\s+virtual\s+(\S+)${ret}\s+(\S+\(.*\))${sig}" =>
        Success(RawMethod("virtual", ret, sig))
      case r"\[\d*\]\s+static\s+(\S+)${ret}\s+(\S+\(.*\))${sig}" =>
        Success(RawMethod("static", ret, sig))
      case r"\[\d*\]\s+(\d+)${counter}\s+(\d+)${offset}(.*)${inst}" =>
        Success(RawInstructionLine(counter, offset, inst.trim(), text))
      case r"\s*" =>
        Failure(MalformedLine(text, Some(EmptyLine)))
      case _ =>
        Failure(MalformedLine(text, None))
    }
  }

  def parseHint(text: String): Try[ResolutionHint] =
    text match {
      case r"<unresolved klass at (\d*)${at}>" =>
        Success(UnresolvedHint(at))
      case r"<(.*)${cls}\.(.*)${method}(\(.*\).*)${desc}>" =>
        Success(MethodHint(className = cls, methodName = method, descriptor = desc))
      case r"<(.*)${cls}\.([^/]*)${field}(/.*)${desc}>" =>
        Success(FieldHint(className = cls, fieldName = field, descriptor = desc))
      case r"<(.*)${name}>" =>
        Success(TypeHint(name = name))
      case _ =>
        Success(TypeHint(text))
    }

  def parseRawInstruction(text: String): Try[RawInstruction] = {
    val parts = text.split("\\s+", 2)
    if (parts.length == 2) {
      Success(RawInstruction(parts(0), parts(1)))
    } else if (parts.length == 1) {
      Success(RawInstruction(parts(0), ""))
    } else {
      Failure(MalformedLine(text, None))
    }
  }

  def parseConstantOperand(text: String): Try[ConstantValue] =
    parseHint(text)
      .map(hint => ConstantValue(None, Some(hint)))
      .orElse(Success(ConstantValue(Some(text), None)))

  def parseLocalIndex(text: String): Try[LocalVariableIndex] =
    Try(text.replace("#", "").toInt)
      .map(LocalVariableIndex)
      .transform(
        s => Success(s),
        t => Failure(MalformedOperands("model.LocalVariableIndex", text, Some(t))))

  def parseBranchOffset(text: String): Try[BranchOffset] =
    Try(text.toInt)
      .map(BranchOffset)
      .transform(
        s => Success(s),
        t => Failure(MalformedOperands("model.BranchOffset", text, Some(t))))

  def parseInvokeDynamic(operands: String): Try[InvokeDynamic] = {
    operands match {
      case r"bsm=(\d+)${bsm}\s*(.*)${rest}" =>
        for {
          bsmIndex <- Try(bsm.toInt)
          constantPoolIndex <- parseConstPoolIdx(rest)
        } yield InvokeDynamic(Some(bsmIndex), constantPoolIndex)
      case _ =>
        parseConstPoolIdx(operands).map(InvokeDynamic(None, _))
    }
  }

  def parseConstPoolIdx(text: String): Try[ConstantPoolIndex] = {
    def parserImpl: Try[ConstantPoolIndex] = {
      val parts = text.split("\\s+", 2)
      if (parts.length == 2) {
        for {
          index <- Try(parts(0).toInt)
          hint <- parseHint(parts(1))
        } yield ConstantPoolIndex(Some(index), Some(hint))
      } else if (parts.length == 1) {
        for {
          index <- Try(parts(0).toInt)
        } yield ConstantPoolIndex(Some(index), None)
      } else {
        Failure(MissingArguments)
      }
    }

    parserImpl match {
      case Failure(t) => Failure(MalformedOperands("model.ConstantPoolIndex", text, Some(t)))
      case s => s
    }
  }

  def baseName(name: String): String = {
    List("^fast_", "^nofast_")
      .foldLeft(name)(
        (z, x) => z.replaceFirst(x, ""))
  }

  def parseInstruction(text: String): Try[Instruction] =
    parseRawInstruction(text).flatMap(parseInstructionFromRaw)

  def parseInstructionFromRaw(raw: RawInstruction): Try[Instruction] = {
    baseName(raw.opcode) match {
      case "return" => Success(Return)
      case "ireturn" => Success(IReturn)
      case "lreturn" => Success(LReturn)
      case "dreturn" => Success(DReturn)
      case "freturn" => Success(FReturn)
      case "areturn" => Success(AReturn)
      case "invokestatic" => parseConstPoolIdx(raw.operands).map(InvokeStatic)
      case "invokevirtual" => parseConstPoolIdx(raw.operands).map(InvokeVirtual)
      case "invokespecial" => parseConstPoolIdx(raw.operands).map(InvokeSpecial)
      case "invokeinterface" => parseConstPoolIdx(raw.operands).map(InvokeInterface)
      case "invokehandle" => parseConstPoolIdx(raw.operands).map(InvokeHandle)
      case "invokedynamic" => parseInvokeDynamic(raw.operands)
      case "idiv" => Success(IDiv)
      case "ldiv" => Success(LDiv)
      case "fdiv" => Success(FDiv)
      case "ddiv" => Success(DDiv)
      case "irem" => Success(IRem)
      case "lrem" => Success(LRem)
      case "frem" => Success(FRem)
      case "drem" => Success(DRem)
      case "iadd" => Success(IAdd)
      case "ladd" => Success(LAdd)
      case "fadd" => Success(FAdd)
      case "dadd" => Success(DAdd)
      case "isub" => Success(ISub)
      case "lsub" => Success(LSub)
      case "fsub" => Success(FSub)
      case "dsub" => Success(DSub)
      case "imul" => Success(IMul)
      case "lmul" => Success(LMul)
      case "fmul" => Success(FMul)
      case "dmul" => Success(DMul)
      case "ishr" => Success(IShr)
      case "iushr" => Success(IUShr)
      case "lshr" => Success(LShr)
      case "lushr" => Success(LUShr)
      case "ishl" => Success(IShl)
      case "iushl" => Success(IUShl)
      case "lshl" => Success(LShl)
      case "lushl" => Success(LUShl)
      case _ => Success(Other(raw.opcode, raw.operands))
      /*
      case "new" => parseConstPoolIdx(raw.operands).map(New)
      case "checkcast" => parseConstPoolIdx(raw.operands).map(CheckCast)
      case "instanceof" => parseConstPoolIdx(raw.operands).map(InstanceOf)
      case r"[a]?ldc.*" | "bipush" | "sipush" =>
      case "arraylength" | "anewarray" | "newarray" =>
      case "multianewarray" =>
      case r"[dialf]store" | r"[dialf]load" =>
      case r".*store_\d" | r".*load_\d" | r".*access_.*" | r".*const_.*" =>
      case r"[abci]aload" | r"[abci]astore" =>
      case r".2." | r"dup.*" =>
      case r".inc" | r"pop\d?" =>
      case r".xor" | r".neg" | r".and" | r".or" =>
      case "monitorenter" | "monitorexit" =>
      case r".cmp[gl]?" =>
      case r"if.*" | "goto" =>
      case r"[dial]?getfield.*" | r"[dial]?putfield.*" | "putstatic" | "getstatic" =>
      */
    }
  }

  def parseSummaryLine(raw: RawSummaryLine): Try[SummaryLine] =
    (for {
      count <- Try(raw.bytecodeCount.toInt)
      duration <- Try(raw.duration.toDouble)
    } yield {
      SummaryLine(count, duration)
    }).transform(
      s => Success(s),
      e => Failure(MalformedLine(raw.text, Some(e))))

  def parseCounterLine(raw: RawCounterLine): Try[CounterLine] =
    (for {
      count <- Try(raw.bytecodeCount.toInt)
    } yield {
      CounterLine(count)
    }).transform(
      s => Success(s),
      e => Failure(MalformedLine(raw.text, Some(e))))

  def parseInstructionLine(raw: RawInstructionLine): Try[InstructionLine] =
    (for {
      counter <- Try(raw.counter.toInt)
      offset <- Try(raw.offset.toInt)
      instruction <- parseInstruction(raw.instruction)
    } yield {
      InstructionLine(instruction, counter, offset)
    }).transform(
      s => Success(s),
      e => Failure(MalformedLine(raw.text, Some(e))))

  def parseTraceLine(text: String): Try[TraceLine] =
    parseRawLine(text).flatMap {
      case RawMethod(invokeType, ret, signature) =>
        Success(MethodEntryLine(invokeType, ret, signature))
      case instruction: RawInstructionLine =>
        parseInstructionLine(instruction)
      case summary: RawSummaryLine =>
        parseSummaryLine(summary)
      case counter: RawCounterLine =>
        parseCounterLine(counter)
    }

  def parseFileStream(source: Source): Iterator[Try[TraceLine]] = {
    source.getLines()
      .zipWithIndex
      .filter(p => !p._1.isBlank)
      .map(p => parseTraceLine(p._1).transform(
        s => Success(s),
        e => Failure(FileParsingError(p._2, e))))
  }

  def parseFile(source: Source): Try[List[TraceLine]] = {
    Try(parseFileStream(source).toList.map(_.get))
  }
}
