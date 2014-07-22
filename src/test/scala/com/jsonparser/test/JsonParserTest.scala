package com.jsonparser.test

import org.specs2.mutable.SpecificationWithJUnit
import com.jsonparser._


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
      val content = Map(("a" -> JsonInt(3)))
      tree === JsonObject(content)
    }

    "parse object with many inta" in {
      val tree = JsonParser.parse("{\"a\":3,\"b\":4}")
      val content = Map(("a" -> JsonInt(3)), ("b" -> JsonInt(4)))
      tree === JsonObject(content)
    }

    "parse object with string" in {
      val tree = JsonParser.parse("{\"a\":\"a\"}")
      val content = Map(("a" -> JsonString("a")))
      tree === JsonObject(content)
    }

    "parse object with true" in {
      val tree = JsonParser.parse("{\"a\":true}")
      val content = Map(("a" -> JsonTrue))
      tree === JsonObject(content)
    }

    "parse object with false" in {
      val tree = JsonParser.parse("{\"a\":false}")
      val content = Map(("a" -> JsonFalse))
      tree === JsonObject(content)
    }

    "parse object with null" in {
      val tree = JsonParser.parse("{\"a\":null}")
      val content = Map(("a" -> JsonNull))
      tree === JsonObject(content)
    }

    "parse object with double" in {
      val tree = JsonParser.parse("{\"a\":1.0}")
      val content = Map(("a" -> JsonDouble(1.0)))
      tree === JsonObject(content)
    }


  }


}
