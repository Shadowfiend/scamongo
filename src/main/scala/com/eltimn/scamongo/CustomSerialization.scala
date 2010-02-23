package com.eltimn.scamongo {

import net.liftweb.json.Formats
import net.liftweb.json.JsonAST._

import scala.collection.mutable.HashMap

import java.lang.reflect.Method

/**
 * Mix in this trait if your object is serializable to a JObject.
 */
trait JObjectSerializable {
  def toJObject : JObject
}

/**
 * Mix in this trait if your object is serializable to a JValue. Note that
 * JObjectSerializable is more appropriate if you serialize to a multi-field
 * object.
 */
trait JValueSerializable {
  def toJValue : JValue
}

trait CustomSerialization[BaseDocument] extends MongoDocumentMeta[BaseDocument] with ClassData {
  override def toJObject(in: BaseDocument)(implicit formats: Formats): JObject = {
    JObject((for ((field, method) <- fieldNameGetterMap if in != null)
      yield JField(field, convertValueToJValue(method.invoke(in)))).toList)
  }

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
      case _ => convertUnknownToJValue(value.asInstanceOf[AnyRef])
    }
  }

  protected def convertUnknownToJValue(value: AnyRef): JValue = {
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
}

}
