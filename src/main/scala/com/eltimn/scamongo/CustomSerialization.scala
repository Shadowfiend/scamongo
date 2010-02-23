package com.eltimn.scamongo {

import net.liftweb.json.{Formats, DefaultFormats}
import net.liftweb.json.JsonAST._

import scala.collection.mutable.HashMap

import java.lang.reflect.Method

/**
 * Mix in this trait if your object is serializable to a JObject.
 */
trait JObjectSerializable {
  def toJObject(implicit formats:Formats) : JObject
}

/**
 * Mix in this trait if your object is serializable to a JValue. Note that
 * JObjectSerializable is more appropriate if you serialize to a multi-field
 * object.
 */
trait JValueSerializable {
  def toJValue(implicit formats:Formats) : JValue
}

/**
 * Mix in this trait to provide a default implementation of toJObject that
 * returns the result of calling toJObject on the MongoDocument's companion.
 */
trait JObjectSerializableByMeta[BaseDocument] extends MongoDocument[BaseDocument] with JObjectSerializable { self: BaseDocument =>
  override def toJObject(implicit formats:Formats) = meta.toJObject(this)
}

trait CustomSerialization[BaseDocument] extends MongoDocumentMeta[BaseDocument] with ClassData {
  override def toJObject(in: BaseDocument)(implicit formats: Formats): JObject = {
    JObject((for ((field, method) <- fieldNameGetterMap if in != null)
      yield JField(field, convertValueToJValue(method.invoke(in)))).toList)
  }

  /**
   * Serializes primitives such as strings, numbers, and booleans to JValues.
   * More complex objects, such as lists and other objects, are delegated to
   * convertComplexToJValue.
   */
  protected def convertValueToJValue(value: Any): JValue = {
    value match {
      case str:String   => JString(str)
      case doub:Double  => JDouble(doub)
      case intVal:Int   => JInt(intVal)
      case bInt:BigInt  => JInt(bInt)
      case bool:Boolean => JBool(bool)
      case Some(option) => convertValueToJValue(option)
      case None         => null
      case null         => null
      case _ => convertComplexToJValue(value.asInstanceOf[AnyRef])
    }
  }

  /**
   * Converts a complex value to a JValue. Complex values are non-primitive
   * objects. These are serialized to null if they cannot be serialized
   * otherwise. If they mix in the JObjectSerializable or JValueSerializable
   * traits, they are serialized using toJObject or toJValue, respectively.
   *
   * Also see TypingSerialization's implementation, which adds List support,
   * and the JObjectSerializableByMeta trait, which, when mixed into a
   * MongoDocument, defauls the toJObject method to call the associated meta
   * object's toJObject method for serialization.
   */
  protected def convertComplexToJValue(value: AnyRef): JValue = {
    value match {
      case jobjectSerializable:JObjectSerializable => jobjectSerializable.toJObject
      case jvalueSerializable:JValueSerializable   => jvalueSerializable.toJValue
      case _ => null
    }
  }
}

trait NullSkippingSerialization[BaseDocument] extends CustomSerialization[BaseDocument] {
  override def toJObject(in: BaseDocument)(implicit formats: Formats): JObject = {
    JObject(super.toJObject(in)(formats).obj.filter { _.value != null })
  }
}

trait TypingSerialization[BaseDocument <: AnyRef] extends CustomSerialization[BaseDocument] {
  override def toJObject(in: BaseDocument)(implicit formats: Formats): JObject = {
    val clazz:Class[_] = in.getClass
    if (unserializableTypes.contains(clazz)) return super.toJObject(in)(formats)

    val className = clazz.getName

    JObject(JField("scamongoType", JString(className)) :: super.toJObject(in)(formats).obj)
  }

  def unserializableTypes: List[Class[_]] = List()

  override protected def convertComplexToJValue(value: AnyRef): JValue = {
    value match {
      case list:List[_] => JArray(list.map { item => convertValueToJValue(item) })
      case _ => super.convertComplexToJValue(value)
    }
  }
}

}
