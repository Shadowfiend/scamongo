package com.eltimn.scamongo {

import net.liftweb.json.Formats
import net.liftweb.json.JsonAST.JObject

import scala.reflect.Manifest
import scala.collection.mutable.HashMap

import java.lang.reflect.{Constructor => JConstructor, Field, Type, Method}

trait CustomDeserialization[BaseDocument] { self:MongoDocumentMeta[BaseDocument] =>
  def create(in:JObject)(implicit formats: Formats) : BaseDocument = {
    fromJObject(in)
  }

  def fromJObject(jobject:JObject) : BaseDocument
}

trait SimpleDeserialization[BaseDocument] extends CustomDeserialization[BaseDocument] {
  self: MongoDocumentMeta[BaseDocument] =>

  private var fieldMethodMap = HashMap.empty[String, Method]

  def fromJObject(jobject:JObject) : BaseDocument = {
    val constructor = this.getClass.getDeclaredConstructor()
    val baseInstance = constructor.newInstance().asInstanceOf[BaseDocument]

    val objectMap = jobject.values
    objectMap.keys foreach { key =>
      try {
        this.getClass.getField(key).set(baseInstance, objectMap(key))
      } catch {
        case exc:NoSuchFieldException => // We ignore this field.
      }
    }

    baseInstance
  }
}

}
