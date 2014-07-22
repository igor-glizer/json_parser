package com.jsonparser.test

import org.specs2.mutable.SpecificationWithJUnit
import com.jsonparser._


/**
 * Created by Igor_Glizer on 7/22/14.
 */
class JsonParserTest extends SpecificationWithJUnit {

  "JsonObject" should {


    "generate empty" in {
      val jsonParser = JsonParser
      val tree = jsonParser.parse("{}")

      tree === JsonObject.empty
    }

    "parse object with single number" in {
      val jsonParser = JsonParser
      val tree = jsonParser.parse("{\"a\",3}")
      val content = Map(("a", JsonNumber(3)))
      tree === JsonObject(content)

    }
  }


}
