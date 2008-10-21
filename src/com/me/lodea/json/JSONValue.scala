package com.me.lodea.json

import scala.collection.Map

sealed abstract class JSONValue

final case class JSONNumber(value: Double) extends JSONValue

final case class JSONBoolean(booleanValue: Boolean) extends JSONValue

final case class JSONString(stringValue: String) extends JSONValue

final case class JSONNull() extends JSONValue

final case class JSONArray(listValue: List[JSONValue]) extends JSONValue

final case class JSONObject(objectValue: Map[String, JSONValue]) extends JSONValue

