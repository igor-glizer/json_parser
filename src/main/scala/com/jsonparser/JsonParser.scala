package com.jsonparser

/**
 * Created by Igor_Glizer on 7/22/14.
 */
object JsonParser {
  def parse(jsonString : String): JsonObject =
    if (jsonString.equals("{}")) JsonObject.empty
    else parseNumberFields(jsonString)

  def parseNumberFields(jsonString : String) = {
    val fields = jsonString.tail.init.split(",")
    var fieldsSeq = Seq[(String, JsonNumber)]()
    for (fieldString <- fields)
    {
      val keyValueArray = fieldString.split(":")
      val key = keyValueArray(0).tail.init
      val value = JsonNumber(keyValueArray(1).toInt)
      fieldsSeq = fieldsSeq :+ (key, value)
    }

    JsonObject(fieldsSeq.toMap)
  }

}