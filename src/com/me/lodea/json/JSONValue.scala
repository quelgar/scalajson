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

}

final case class JSONNumber(value: Double) extends JSONValue

final case class JSONBoolean(booleanValue: Boolean) extends JSONValue

final case class JSONString(stringValue: String) extends JSONValue

final case class JSONNull() extends JSONValue

final case class JSONArray(listValue: List[JSONValue]) extends JSONValue

final case class JSONObject(objectValue: Map[String, JSONValue]) extends JSONValue


object JSONValue {

  implicit def fromInt(n: Int) = JSONNumber(n)

  implicit def fromFloat(n: Float) = JSONNumber(n)

  implicit def fromDouble(n: Double) = JSONNumber(n)

  implicit def fromBoolean(b: Boolean) = if (b) JSON.TRUE else JSON.FALSE

  implicit def fromString(s: String) = JSONString(s)

  implicit def fromMap(m: Map[String, JSONValue]) = JSONObject(m)

  implicit def fromList(l: List[JSONValue]) = JSONArray(l)

}