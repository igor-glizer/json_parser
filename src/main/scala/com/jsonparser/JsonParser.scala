package com.jsonparser

/**
 * Created by Igor_Glizer on 7/22/14.
 */
object JsonParser {
  def parse(jsonString : String): JsonObject =
    if (jsonString.equals("{}")) JsonObject.empty
    else parseNumberFields(jsonString)

  def parseNumberFields(jsonString : String) = {
    val fields = extractFields(jsonString)
    JsonObject(fields.map(extractKeyValue).toMap)
  }

  def extractKeyValue(fieldString: String): (String, JsonNumber) = {
    val keyValueArray = fieldString.split(":")
    val key = removeEnclosingSymbols(keyValueArray(0))
    val value = JsonNumber(keyValueArray(1).toInt)
    (key, value)
  }

  def extractFields(json: String) = removeEnclosingSymbols(json).split(",")

  def removeEnclosingSymbols(string: String) = string.tail.init
}