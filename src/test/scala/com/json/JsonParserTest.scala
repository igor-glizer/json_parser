package com.json

import org.specs2.mutable.SpecificationWithJUnit


/**
 * Created by Igor_Glizer on 7/22/14.
 */
class JsonParserTest extends SpecificationWithJUnit {

  "JsonObject" should {


    "generate empty" in {
      val tree = JsonParser.parse("{}")
      tree === JsonObject.empty
    }

    "parse object with int" in {
      val tree = JsonParser.parse("{\"a\":3}")
      val content = Map("a" -> JsonInt(3))
      tree === JsonObject(content)
    }

    "parse object with many ints" in {
      val tree = JsonParser.parse("{\"a\":3,\"b\":4}")
      val content = Map("a" -> JsonInt(3), "b" -> JsonInt(4))
      tree === JsonObject(content)
    }

    "parse object with string" in {
      val tree = JsonParser.parse("{\"a\":\"a\"}")
      val content = Map("a" -> JsonString("a"))
      tree === JsonObject(content)
    }

    "parse object with true" in {
      val tree = JsonParser.parse("{\"a\":true}")
      val content = Map("a" -> JsonTrue)
      tree === JsonObject(content)
    }

    "parse object with false" in {
      val tree = JsonParser.parse("{\"a\":false}")
      val content = Map("a" -> JsonFalse)
      tree === JsonObject(content)
    }

    "parse object with null" in {
      val tree = JsonParser.parse("{\"a\":null}")
      val content = Map("a" -> JsonNull)
      tree === JsonObject(content)
    }

    "parse object with double" in {
      val tree = JsonParser.parse("{\"a\":1.0}")
      val content = Map("a" -> JsonDouble(1.0))
      tree === JsonObject(content)
    }

    "parse object with empty array" in {
      val tree = JsonParser.parse("{\"a\":[]}")
      val content = Map(("a" -> JsonArray()))
      tree === JsonObject(content)
    }

    "parse object with one int array" in {
      val tree = JsonParser.parse("{\"a\":[1]}")
      val content = Map("a" -> JsonArray(JsonInt(1)))
      tree === JsonObject(content)
    }

    "parse object with one string array" in {
      val tree = JsonParser.parse("{\"a\":[\"a\"]}")
      val content = Map("a" -> JsonArray(JsonString("a")))
      tree === JsonObject(content)
    }

    "parse object with many ints array" in {
      val tree = JsonParser.parse("{\"a\":[1,2]}")
      val content = Map("a" -> JsonArray(JsonInt(1), JsonInt(2)))
      tree === JsonObject(content)
    }

    "parse object with inner empty object" in {
      val tree = JsonParser.parse("{\"a\":{}}")
      val content = Map("a" -> JsonObject.empty)
      tree === JsonObject(content)
    }

    "parse object with int value inner object" in {
      val tree = JsonParser.parse("{\"a\":{\"b\":1}}")
      val content = Map("a" -> JsonObject(Map("b" -> JsonInt(1))))
      tree === JsonObject(content)
    }

    "parse object with string value inner object" in {
      val tree = JsonParser.parse("{\"a\":{\"b\":\"c\"}}")
      val content = Map("a" -> JsonObject(Map("b" -> JsonString("c"))))
      tree === JsonObject(content)
    }
  }


}
