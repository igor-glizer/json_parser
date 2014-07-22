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

    "parse object with number" in {
      val tree = JsonParser.parse("{\"a\":3}")
      val content = Map(("a" -> JsonNumber(3)))
      tree === JsonObject(content)
    }

    "parse object with many numbers" in {
      val tree = JsonParser.parse("{\"a\":3,\"b\":4}")
      val content = Map(("a" -> JsonNumber(3)))
      tree === JsonObject(content)
    }


  }


}
