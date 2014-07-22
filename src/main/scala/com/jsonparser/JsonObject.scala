package com.jsonparser

/**
 * Created by Igor_Glizer on 7/22/14.
 */
sealed trait JsonValue
case class JsonInt(value : Int) extends JsonValue
case class JsonDouble(value : Double) extends JsonValue
case class JsonString(value : String) extends JsonValue
case object JsonTrue extends JsonValue
case object JsonFalse extends JsonValue
case object JsonNull extends JsonValue

case class JsonObject(jsonFields : Map[String, JsonValue]) {

}

object JsonObject {
  def empty : JsonObject = new JsonObject(Map())
}