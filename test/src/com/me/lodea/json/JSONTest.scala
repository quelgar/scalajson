package com.me.lodea.json


import java.io.{InputStreamReader, FileInputStream}

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
  }

}