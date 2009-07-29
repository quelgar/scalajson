package com.me.lodea.json

import java.io.PrintWriter
import scala.collection.mutable.Map
import scala.util.parsing.combinator.JavaTokenParsers
import java.util.regex.Pattern
import scala.util.parsing.input.Reader


class JSON extends JavaTokenParsers {

  import JSON._

  override val skipWhitespace = false

  def json: Parser[JSONValue] = obj | array

  def obj: Parser[JSONValue] = token("{")~>repsep(member, token(","))<~token("}") ^^ ((member: Iterable[(String, JSONValue)]) => JSONObject(Map() ++ member))

  def array: Parser[JSONValue] = token("[")~>repsep(value, token(","))<~token("]") ^^ (JSONArray(_))

  def member: Parser[(String, JSONValue)] = jsonString~token(":")~value ^^ { case name~":"~value => (name, value) }

  def value: Parser[JSONValue] = (
      json
      | jsonString ^^ (JSONString(_))
      | floatingPointNumber ^^ ((s: String) => JSONNumber(s.toDouble))
      | "null" ^^ (s => NULL)
      | "false" ^^ (s => FALSE)
      | "true" ^^ (s => TRUE)
      )

  def token(s: String): Parser[String] = "\\s*".r~>s<~"\\s*".r

  def jsonString: Parser[String] = "\""~>rep(escapeSeq | validStringChar)<~"\"" ^^ { _ mkString }

  def escapeSeq: Parser[String] = (
      "\\\"" ^^^ "\""
      | "\\\\" ^^^ "\\"
      | "\\/" ^^^ "/"
      | "\\b" ^^^ "\b"
      | "\\f" ^^^ "\f"
      | "\\n" ^^^ "\n"
      | "\\r" ^^^ "\r"
      | "\\t" ^^^ "\t"
      | "\\u"~>unicodeSeq
      )

  def unicodeSeq: Parser[String] = hexDigit~hexDigit~hexDigit~hexDigit ^^ {
    case a~b~c~d => {
      String.valueOf((a << 12 | b << 8 | c << 4 | d).asInstanceOf[Char])
    }
  }

  def hexDigit: Parser[Int] = "[a-fA-F0-9]".r ^^ { stringWrapper(_)(0).asDigit }

  def validStringChar: Parser[String] = """[^\\"\p{javaISOControl}]""".r

}

object JSON {

  val NULL = JSONNull()

  val FALSE = JSONBoolean(false)

  val TRUE = JSONBoolean(true)

  def parse(in: CharSequence) = {
    val parser = new JSON()
    parser.parse(parser.json, in)
  }

  def parse(in: java.io.Reader) = {
    val parser = new JSON()
    parser.parse(parser.json, in)
  }

  def parse(in: Reader[Char]) = {
    val parser = new JSON()
    parser.parse(parser.json, in)
  }

  def fromNumber(n: Double) = JSONNumber(n)

  def fromNumber(n: Int) = JSONNumber(n)

  def print(value: JSONValue, writer: PrintWriter) {
    new JSONPrinter(writer).print(value)
  }

  def prettyPrint(value: JSONValue, writer: PrintWriter, indent: Int) {
    new PrettyPrinter(writer, indent).print(value)
    writer.println
  }

  def prettyPrint(value: JSONValue, writer: PrintWriter) { prettyPrint(value, writer, 4) }

  def shallowPrint(value: JSONValue, writer: PrintWriter) {
    new ShallowPrinter(writer).print(value)
  }

  private class JSONPrinter(protected val writer: PrintWriter) {

    def print(value: JSONValue): Unit = value match {
      case JSONObject(map) => {
        writer.print('{')
        println
        indent
        var first = true
        for ((name, subValue) <- map) {
          if (!first) {
            writer.print(',')
            println
          }
          first = false
          printIndent
          printJSONString(name)
          writer.print(':')
          printSpace
          print(subValue)
        }
        outdent
        println
        printIndent
        writer.print('}')
      }
      case JSONArray(array) => {
        writer.print('[')
        println
        indent
        var first = true
        for (subValue <- array) {
          if (!first) {
            writer.print(',')
            println
          }
          first = false
          printIndent
          print(subValue)
        }
        outdent
        println
        printIndent
        writer.print(']')
      }
      case JSONNull() => writer.print("null")
      case JSONBoolean(true) => writer.print("true")
      case JSONBoolean(false) => writer.print("false")
      case JSONNumber(n) => writer.print(n)
      case JSONString(s) => printJSONString(s)
    }

    private def printJSONString(s: String) {
      writer.print('"')
      for (c <- s) {
        c match {
          case '"' => writer.print("\\\"")
          case '\\' => writer.print("\\\\")
          case '\b' => writer.print("\\b")
          case '\f' => writer.print("\\f")
          case '\n' => writer.print("\\n")
          case '\r' => writer.print("\\r")
          case '\t' => writer.print("\\t")
          case x if x.isControl => {
            writer.print("\\u")
            writer.print(((x.toInt & 0xF000) >>> 12).toHexString)
            writer.print(((x.toInt & 0x0F00) >>> 8).toHexString)
            writer.print(((x.toInt & 0x00F0) >>> 4).toHexString)
            writer.print((x.toInt & 0x000F).toHexString)
          }
          case x => writer.print(x)
        }
      }
      writer.print('"')
    }

    protected def indent { }

    protected def outdent { }

    protected def println { }

    protected def printIndent { }

    protected def printSpace { }

  }

  private class PrettyPrinter(writer: PrintWriter, indentBy: Int) extends JSONPrinter(writer) {

    var currentIndent = 0

    override protected def printIndent {
      for (_ <- 1 to currentIndent) {
        writer.print(' ')
      }
    }
    override protected def outdent {
      currentIndent -= indentBy
    }

    override protected def println {
      writer.println
    }

    override protected def printSpace {
      writer.print(' ')
    }

    override protected def indent {
      currentIndent += indentBy
    }

  }

  private class ShallowPrinter(writer: PrintWriter) extends JSONPrinter(writer) {

    private var printed = false

    override def print(value: JSONValue): Unit = value match {
      case JSONObject(_) if printed => writer.print("<OBJECT\u2026>")
      case JSONArray(_) if printed => writer.print("<ARRAY\u2026>")
      case _ => { printed = true; super.print(value) }
    }

    override protected def println { printSpace }

    override protected def printSpace { writer.print(' ') }

  }

}
