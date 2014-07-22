package com.jsonparser

/**
 * Created by Igor_Glizer on 7/22/14.
 */
object JsonParser {
  def parse(jsonString : String): JsonObject =
    if (jsonString.equals("{}")) JsonObject.empty
    else parseFields(jsonString)

  private def parseFields(jsonString : String) = {
    val fields = extractFields(jsonString)
    JsonObject(fields.map(extractKeyValue).toMap)
  }

  private def extractKeyValue(fieldString: String) = {
    val keyValueArray = fieldString.split(":")
    val key = removeEnclosingSymbols(keyValueArray(0))
    val value = parseValue(keyValueArray(1))
    (key, value)
  }

  private def parseValue(value : String) = {
    value.head match {
      case '"' => JsonString(removeEnclosingSymbols(value))
      case c if c.isDigit || c == '+' || c == '-' => JsonNumber(value.toInt)
      case _ => JsonTrue
    }
  }

  private def extractFields(json: String) = removeEnclosingSymbols(json).split(",")

  private def removeEnclosingSymbols(string: String) = string.tail.init

}