package com.me.lodea.json


import java.io.{InputStreamReader, FileInputStream}

object JSONTest {

  def main(args: Array[String]) {
    val result = JSON.parse(new InputStreamReader(new FileInputStream(args(0))))
    val json = result.get
    json match {
      case JSONObject(values) => {
        printf("Object: %s%n", values)
        values("an int") match {
          case JSONNumber(666) => println("Matched integer 666!")
        }
        values("an int") match {
          case JSONNumber(667) => error("Should not match")
          case _ => println("Didn't match 667")
        }
        values("Description") match {
          case JSONString(x) => printf("Description = '%s'%n", x)
        }
        values("a decimal") match {
          case JSONNumber(42.666) => printf("Matched 42.666") 
        }
      }
    }
  }

}