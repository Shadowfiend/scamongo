package com.eltimn.scamongo {

import net.liftweb.json.Formats
import net.liftweb.json.JsonAST._

import scala.collection.mutable.HashMap

import java.lang.reflect.Method

trait NullSkippingSerialization[BaseDocument] extends MongoDocumentMeta[BaseDocument] {
  private val clazz = Class.forName(
    this.getClass.getName.substring(0, this.getClass.getName.length - 1))
  // Map field names to their getter methods, but only if they have a setter method.
  private val fieldNameMethodMap =
    new HashMap[String, Method] ++
          (for (method <- clazz.getMethods;
                methodName = method.getName if methodName.length > 4;
                fieldName = methodName.substring(0, methodName.length - 4) if methodName.endsWith("_$eq");
                if !fieldName.contains("$"))
            yield (fieldName -> clazz.getMethod(fieldName)))

  override def toJObject(in: BaseDocument)(implicit formats: Formats): JObject = {
    JObject((for ((field, method) <- fieldNameMethodMap if in != null)
      yield JField(field, convertValueToJValue(method.invoke(in)))).toList.filter { _.value != null })
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
    val clazz = value.getClass
    clazz.getMethods.find { method => method.getName == "toJObject" || method.getName == "toJValue" } match {
      case Some(method) => method.invoke(value).asInstanceOf[JValue]
      case None => null
    }
  }
}

}
