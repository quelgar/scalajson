package com.me.lodea.json


import java.io.{InputStreamReader, FileInputStream, OutputStreamWriter, PrintWriter}

object JSONTest {

  def main(args: Array[String]) {
    val result = JSON.parse(new InputStreamReader(new FileInputStream(args(0))))
    val json = result.get
    (json: @unchecked) match {
      case JSONObject(values) => {
        printf("Object: %s%n", values)
        (values("an int"): @unchecked) match {
          case JSONNumber(666) => println("Matched integer 666!")
        }
        (values("an int"): @unchecked) match {
          case JSONNumber(667) => error("Should not match")
          case _ => println("Didn't match 667")
        }
        (values("Description"): @unchecked) match {
          case JSONString(x) => printf("Description = '%s'%n", x)
        }
        (values("a decimal"): @unchecked) match {
          case JSONNumber(42.666) => println("Matched 42.666") 
        }
        (values("Image"): @unchecked) match {
          case JSONObject(imageValues) => {
            (imageValues("IDs"): @unchecked) match {
              case JSONArray(List(JSONNumber(116), JSONNumber(x), _*)) => printf("Matched IDs list, 2nd value = %d%n", x.toInt)
            }
          }
        }
      }
    }

    val testJSON = JSONObject(Map(
      "a" -> JSONNumber(666),
      "2nd \u007F" -> JSONArray(List(5, 4, 9).map(JSON.fromNumber _)),
      "more" -> JSONObject(Map(
        "inner" -> JSON.TRUE,
        "null value" -> JSON.NULL
        )),
      "strange" -> JSONString("copyright: \u00A9 and \ttab")
      ))

    val pw = new PrintWriter(Console.out)
    JSON.prettyPrint(testJSON, pw)
    pw.flush

    // take advantage of implicit conversions, see object JSONValue
    val testImplicit = JSONObject(Map(
      "int" -> 4,
      "null" -> JSON.NULL,
      "boolean" -> true,
      "false" -> false,
      "float" -> 4.5F,
      "string" -> "Some string value",
      "array" -> List("a", "b", "c"),
      "number array" -> List(4, 5, 6),
      "object" -> Map("sub1" -> 3, "sub2" -> false)
      ))
    JSON.print(testImplicit, pw)
    pw.println
    pw.flush

    pw.println(testImplicit)
    pw.println(testImplicit.objectValue("float"))
    pw.println(testImplicit.objectValue("string"))
    pw.println(testImplicit.objectValue("array"))
    pw.println(testImplicit.objectValue("boolean"))
    pw.println(JSON.NULL)
    pw.flush
  }

}
