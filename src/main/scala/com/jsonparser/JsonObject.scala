package com.jsonparser

/**
 * Created by Igor_Glizer on 7/22/14.
 */
sealed trait JsonValue
case class JsonNumber(value : Int) extends JsonValue
case class JsonString(value : String) extends JsonValue
case object JsonTrue extends JsonValue

case class JsonObject(jsonFields : Map[String, JsonValue]) {

}

object JsonObject {
  def empty : JsonObject = new JsonObject(Map())
}