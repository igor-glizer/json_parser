package com.jsonparser

/**
 * Created by Igor_Glizer on 7/22/14.
 */

case class JsonNumber(value : Int)

case class JsonObject(jsonFields : Map[String, JsonNumber]) {

}

object JsonObject {
  def empty : JsonObject = new JsonObject(Map())
}