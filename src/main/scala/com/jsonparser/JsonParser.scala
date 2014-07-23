package com.jsonparser

/**
 * Created by Igor_Glizer on 7/22/14.
 */
object JsonParser {

  def parse(jsonString : String): JsonObject =
    if (jsonString.equals("{}")) JsonObject.empty
    else parseFields(jsonString)

  private def parseFields(jsonString : String) = {
    val fields = extractFields(removeObjectBraces(jsonString))
    JsonObject(fields.map(extractKeyValue).toMap)
  }

  private def extractKeyValue(fieldString: String) = {
    val keyValueArray = fieldString.split(":")
    val key = removeStringQuotes(keyValueArray(0))
    val value = parseValue(keyValueArray(1))
    (key, value)
  }

  private def parseValue(value : String) : JsonValue = {
    value.head match {
      case '"' => JsonString(removeStringQuotes(value))
      case '[' => parseArray(value)
      case c if c.isDigit || c == '+' || c == '-' => parseNumber(value)
      case _ => parseLiteral(value)
    }
  }

  private def extractFields(json: String)  =
  {
    val splittedByComma = json.split(",")
    var fields = Seq[String]()
    var arrayBraceOpeners = 0
    var arrayBraceClosers = 0
    var lastItem = ""
    for (currItem <- splittedByComma){
      arrayBraceOpeners = arrayBraceOpeners + currItem.count(_ == '[')
      arrayBraceClosers= arrayBraceClosers + currItem.count(_ == ']')
      lastItem = if (lastItem.nonEmpty) lastItem + "," + currItem else currItem
      if (arrayBraceOpeners == arrayBraceClosers)
      {
        fields = fields :+ lastItem
        lastItem = ""
        arrayBraceOpeners = 0
        arrayBraceClosers = 0
      }
    }
    fields
  }

  private def parseLiteral(value: String) = value match {
    case "true" => JsonTrue
    case "false" => JsonFalse
    case "null" => JsonNull
  }

  private def parseNumber(value: String) = {
    if (value.contains("."))
      JsonDouble(value.toDouble)
    else
      JsonInt(value.toInt)
  }

  private def parseArray(value: String) = {
    val arrayString = removeArrayBraces(value)
    if (arrayString.isEmpty)
      JsonArray()
    else
      JsonArray((extractFields(arrayString).map(parseValue):_*))
  }

  private def removeStringQuotes(string: String) = removeEnclosingSymbols(string, '"', '"')

  private def removeObjectBraces(string: String) = removeEnclosingSymbols(string, '{', '}')

  private def removeArrayBraces(string: String) = removeEnclosingSymbols(string, '[', ']')

  private def removeEnclosingSymbols(string: String, firstChar : Char, lastChar : Char) = {
    if (string.head == firstChar && string.last == lastChar)
      string.tail.init
    else
      throw new ParsingException(s"$string should have started with $firstChar and ended with $lastChar")
  }

  class ParsingException(value : String) extends RuntimeException(value)

}