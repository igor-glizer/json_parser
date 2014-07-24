package com.json

import org.specs2.mutable.SpecificationWithJUnit

/**
 * Created by Igor_Glizer on 7/24/14.
 */
class JsonPrinterTest extends SpecificationWithJUnit {
  "JsonPrinter" should {

    "print empty" in {
      val emptyJson = "{}"
      JsonPrinter.print(JsonParser.parse(emptyJson)) must_== emptyJson
    }
  }

}
