package parser

class ParserError(msg: String) extends Throwable {
  override def toString: String = "%s".format(msg)
}

object EmptyLine
  extends ParserError("Empty Line")

object MissingArguments
  extends ParserError("Expected arguments, but found none")

case class MalformedOperands(kind: String, text: String, inner: Option[Throwable])
  extends ParserError(
    "Failed to parse operands of type '%s' from '%s'. %s".format(
      kind,
      text,
      inner.map("-> %s".format(_)).getOrElse("")))

case class MalformedLine(text: String, inner: Option[Throwable])
  extends ParserError(
    "Failed to parse line: '%s'. %s".format(
      text,
      inner.map("-> %s".format(_)).getOrElse("")))

case class FileParsingError(lineNumber: Int, inner: Throwable) extends Throwable {
  override def toString: String = "Line %d: %s".format(lineNumber, inner)
}