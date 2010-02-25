package com.eltimn.scamongo {

import net.liftweb.json.{Formats, DefaultFormats}
import net.liftweb.json.JsonAST._

import scala.collection.mutable.HashMap

import java.lang.reflect.Method

/**
 * Mix in this trait if your object is serializable to a JObject.
 */
trait JObjectSerializable {
  def toJObject() : JObject = toJObject(DefaultFormats)
  def toJObject(implicit formats:Formats) : JObject
}

/**
 * Mix in this trait if your object is serializable to a JValue. Note that
 * JObjectSerializable is more appropriate if you serialize to a multi-field
 * object.
 */
trait JValueSerializable {
  def toJValue() : JValue = toJValue(DefaultFormats)
  def toJValue(implicit formats:Formats) : JValue
}

/**
 * Mix in this trait to provide a default implementation of toJObject that
 * returns the result of calling toJObject on the MongoDocument's companion.
 */
trait JObjectSerializableByMeta[BaseDocument] extends MongoDocument[BaseDocument] with JObjectSerializable { self: BaseDocument =>
  def toJObject(implicit formats:Formats) = meta.toJObject(this)
}

trait JValueSerialization {
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
      case jobjectSerializable:JObjectSerializable => jobjectSerializable.toJObject()
      case jvalueSerializable:JValueSerializable   => jvalueSerializable.toJValue()
      case _ => null
    }
  }
}

/**
 * Mix in this trait to provide a default implementation of toJObject that uses the
 * same method of serialiation that CustomSerialization uses for MongoDocuments.
 *
 * This trait should be used with non-MongoDocument classes. MongoDocument
 * classes should delegate JObject conversion to their companion object, using
 * JObjectSerializableByMeta.
 */
trait JObjectBasicSerializable extends JObjectSerializable with JValueSerialization with ClassData {
  override def toJObject(implicit formats:Formats) = {
    JObject((for ((field, method) <- fieldNameGetterMap)
      yield JField(field, convertValueToJValue(method.invoke(this)))).toList)
  }
}

/**
 * Same as NullSkippingSerializable, but for classes instead of companion objects.
 */
trait JObjectNullSkippingSerializable extends JObjectBasicSerializable {
  override def toJObject(implicit formats:Formats) = {
    JObject(super.toJObject(formats).obj.filter { _.value != null })
  }
}

/**
 * Same as TypingSerializable, but for classes instead of companion objects,
 * and without handling for lists.
 */
trait JObjectTypingSerializable extends JObjectBasicSerializable {
  override def toJObject(implicit formats: Formats): JObject = {
    val className = clazz.getName

    JObject(JField("scamongoType", JString(className)) :: super.toJObject(formats).obj)
  }
}

/**
 * Mix in this trait to provide a companion implementation of toJObject for
 * MongoDocumentMeta objects. This implementation replaces the default
 * MongoDocumentMeta implementation to do simple field-based serialization.
 */
trait CustomSerialization[BaseDocument] extends MongoDocumentMeta[BaseDocument] with ClassDataForObject with JValueSerialization {
  override def toJObject(in: BaseDocument)(implicit formats: Formats): JObject = {
    JObject((for ((field, method) <- fieldNameGetterMap if in != null)
      yield JField(field, convertValueToJValue(method.invoke(in)))).toList)
  }
}

/**
 * Mix in this trait to provide a MongoDocumentMeta implementation of
 * CustomSerialization that also omits all fields with null values.
 * CustomSerialization inserts these into the database with a null value
 * instead of omitting them entirely.
 */
trait NullSkippingSerialization[BaseDocument] extends CustomSerialization[BaseDocument] {
  override def toJObject(in: BaseDocument)(implicit formats: Formats): JObject = {
    JObject(super.toJObject(in)(formats).obj.filter { _.value != null })
  }
}

/**
 * Mix in this trait to provide a MongoDocumentMeta implementation of
 * CustomSerialization that includes the name of the class being serialized
 * alongside its other data in a scamongoType field. This also allows the
 * serialization of list fields, which this trait also provides.
 *
 * Types whose class name should not be serialized can be provided by
 * overriding the unserializableTypes method to return those types. Note that
 * when deserializing these, they will be deserialized with the base document's
 * fromJObject method. This is thus best suited to not include the type on the
 * associated document, but serialize the types of any subclasses.
 */
trait TypingSerialization[BaseDocument <: AnyRef] extends CustomSerialization[BaseDocument] {
  override def toJObject(in: BaseDocument)(implicit formats: Formats): JObject = {
    val clazz:Class[_] = in.getClass
    if (unserializableTypes.contains(clazz)) return super.toJObject(in)(formats)

    val className = clazz.getName
    val fieldGetterMap = fieldGetterMapForClass(clazz)

    val jobject = JObject((for ((field, method) <- fieldGetterMap)
      yield JField(field, convertValueToJValue(method.invoke(in)))).toList)

    JObject(JField("scamongoType", JString(className)) :: jobject.obj)
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
