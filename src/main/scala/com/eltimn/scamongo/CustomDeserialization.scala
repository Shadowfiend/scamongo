package com.eltimn.scamongo {

import net.liftweb.json.Formats
import net.liftweb.json.JsonAST.JObject

import scala.collection.mutable.HashMap

import java.lang.reflect.{Constructor => JConstructor, Field, Type, Method}

trait CustomDeserialization[BaseDocument] extends MongoDocumentMeta[BaseDocument] {
  override def create(in: JObject)(implicit formats: Formats): BaseDocument = {
    fromJObject(in)
  }

  def fromJObject(jobject: JObject): BaseDocument
}

trait SimpleDeserialization[BaseDocument] extends CustomDeserialization[BaseDocument] {
  private val clazz = Class.forName(
    this.getClass.getName.substring(0, this.getClass.getName.length - 1))
  private val constructor = clazz.getDeclaredConstructor()
  // Map field names to their setter methods.
  private val fieldNameMethodMap =
    new HashMap[String, Method] ++
          (for (method <- clazz.getMethods;
                methodName = method.getName
                if methodName.endsWith("_$eq"))
            yield (methodName.substring(0, methodName.length - 4) -> method))

  def fromJObject(jobject: JObject): BaseDocument = {
    val baseInstance = constructor.newInstance().asInstanceOf[BaseDocument]

    val objectMap = jobject.values
    objectMap.keys foreach { key =>
      fieldNameMethodMap.get(key) match {
        case Some(fieldSetter) =>
          try {
            fieldSetter.invoke(baseInstance, objectMap(key).asInstanceOf[Object])
          } catch {
            // Try to see if we tried to set the value where it needed an option
            // for the value.
            case _:IllegalArgumentException => fieldSetter.invoke(baseInstance, Some(objectMap(key)))
          }
        case None => // Don't do anything with this field.
      }
    }

    baseInstance
  }
}

}
