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
  private val clazz = Class.forName(
    this.getClass.getName.substring(0, this.getClass.getName.length - 1))
  private val constructor = clazz.getDeclaredConstructor()
  private val fieldNameMethodMap =
    new HashMap[String, Method] ++
          (for (method <- clazz.getMethods;
               methodName = method.getName
               if methodName.endsWith("_$eq"))
            yield (methodName.substring(0, methodName.length - 4) -> method))

  def fromJObject(jobject:JObject) : BaseDocument = {
    val baseInstance = constructor.newInstance().asInstanceOf[BaseDocument]

    val objectMap = jobject.values
    println(fieldNameMethodMap)
    objectMap.keys foreach { key =>
      fieldNameMethodMap.get(key) match {
        case Some(fieldSetter) => fieldSetter.invoke(baseInstance, objectMap(key).asInstanceOf[Object])
        case None              => // Don't do anything with this field.
      }
    }

    baseInstance
  }
}

}
