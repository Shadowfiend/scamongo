package com.eltimn.scamongo {

import net.liftweb.json.Formats
import net.liftweb.json.JsonAST.JObject

import scala.reflect.Manifest
import scala.collection.mutable.HashMap

import java.lang.reflect.{Constructor => JConstructor, Field, Type, Method}

trait CustomDeserialization[BaseDocument] extends MongoDocumentMeta[BaseDocument] {
  override def create(in:JObject)(implicit formats: Formats) : BaseDocument = {
    fromJObject(in)
  }

  def fromJObject(jobject:JObject) : BaseDocument
}

trait SimpleDeserialization[BaseDocument] extends CustomDeserialization[BaseDocument] {
  private var fieldMethodMap = HashMap.empty[String, Method]

  def fromJObject(jobject:JObject) : BaseDocument = {
    val className = this.getClass.getName
    val constructor = Class.forName(className.substring(0, className.length - 1)).getDeclaredConstructor()
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
