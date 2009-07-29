package com.me.lodea.json

import java.io.{PrintWriter, StringWriter}
import scala.collection.Map

sealed abstract class JSONValue {

  override def toString: String = {
    val sw = new StringWriter(300)
    val pw = new PrintWriter(sw)
    JSON.shallowPrint(this, pw)
    pw.close
    sw.toString
  }

  def number = this match {
    case JSONNumber(n) => Some(n)
    case _ => None
  }

  def boolean = this match {
    case JSONBoolean(b) => Some(b)
    case _ => None
  }

  def string = this match {
    case JSONString(s) => Some(s)
    case _ => None
  }

}

final case class JSONNumber(value: Double) extends JSONValue

final case class JSONBoolean(booleanValue: Boolean) extends JSONValue

final case class JSONString(stringValue: String) extends JSONValue

final case class JSONNull() extends JSONValue

final case class JSONArray(listValue: List[JSONValue]) extends JSONValue

final case class JSONObject(objectValue: Map[String, JSONValue]) extends JSONValue {

    def number(name: String) = for {
        value <- objectValue.get(name)
        number <- value.number
    } yield number

    def number(name: String, default: => Double) = objectValue.getOrElse(name, JSONNumber(default)).number.getOrElse(default)

    def number(name: String, default: => Int) = objectValue.getOrElse(name, JSONNumber(default)).number.getOrElse(default.toDouble).toInt

    def boolean(name: String) = for {
        value <- objectValue.get(name)
        b <- value.boolean
    } yield b

    def boolean(name: String, default: => Boolean) = objectValue.getOrElse(name, JSONBoolean(default)).boolean.getOrElse(default)

    def string(name: String) = for {
        value <- objectValue.get(name)
        s <- value.string
    } yield s

    def string(name: String, default: => String) = objectValue.getOrElse(name, JSONString(default)).string.getOrElse(default)

}


object JSONValue {

  implicit def fromInt(n: Int) = JSONNumber(n)

  implicit def fromFloat(n: Float) = JSONNumber(n)

  implicit def fromDouble(n: Double) = JSONNumber(n)

  implicit def fromBoolean(b: Boolean) = if (b) JSON.TRUE else JSON.FALSE

  implicit def fromString(s: String) = JSONString(s)

  implicit def fromMap(m: Map[String, JSONValue]) = JSONObject(m)

  implicit def fromList(l: List[JSONValue]) = JSONArray(l)

}