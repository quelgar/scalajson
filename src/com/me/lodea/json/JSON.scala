package com.me.lodea.json

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

  def validStringChar: Parser[String] = """[^\\"\p{Cntrl}]""".r

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

}

