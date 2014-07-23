package com.jsonparser

/**
 * Created by Igor_Glizer on 7/22/14.
 */
object JsonParser {

  def parse(jsonString : String): JsonObject =
    if (isJsonEmpty(jsonString)) JsonObject.empty
    else parseToObject(jsonString)

  private def isJsonEmpty(jsonString: String) = jsonString.equals("{}")

  private def parseToObject(jsonString : String) = {
    val fields = extractElements(removeObjectBraces(jsonString), ',')
    JsonObject(fields.map(extractKeyValue).toMap)
  }

  private def extractKeyValue(fieldString: String) = {
    val keyValueArray = extractElements(fieldString, ':')
    val key = removeStringQuotes(keyValueArray(0))
    val value = parseValue(keyValueArray(1))
    (key, value)
  }

  private def parseValue(value : String) : JsonValue = {
    value.head match {
      case '"' => JsonString(removeStringQuotes(value))
      case '[' => parseArray(value)
      case '{' => parse(value)
      case isNumberBeginning => parseNumber(value)
      case _ => parseLiteral(value)
    }
  }

  def isNumberBeginning(c: Char): Boolean = c.isDigit || c == '+' || c == '-'


  private def extractElements(jsonPart: String, splitChar : Char)  =
  {
    val splitPartialElementsByComma = jsonPart.split(splitChar)

    case class ElementsAccumulator(bracesDiff : Int = 0, accumulatedElement : String = "", elements : Seq[String] = Seq()){
      def accumulate(partialElement : String ) = {
        val newBracesDiff = this.bracesDiff + (partialElement.count(isOpenBrace) - partialElement.count(isCloseBrace))
        if (newBracesDiff == 0)
          completeAccumulationOfElement(partialElement)
        else
          continueAccumulationOfElement(partialElement,newBracesDiff)
      }
      def completeAccumulationOfElement(curValue : String) = ElementsAccumulator(elements = elements :+ (accumulatedElement + curValue))
      def continueAccumulationOfElement(curValue : String, bracesDiff : Int) = ElementsAccumulator(bracesDiff, accumulatedElement + curValue + splitChar, elements)

      private def isOpenBrace(c: Char): Boolean =  c == '[' || c == '{'
      private def isCloseBrace(c: Char): Boolean =  c == ']' || c == '}'
    }

    splitPartialElementsByComma.foldLeft(ElementsAccumulator())(
      (accumulatorForFields, partialElement) => accumulatorForFields.accumulate(partialElement)).elements
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
      JsonArray((extractElements(arrayString, ',').map(parseValue):_*))
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