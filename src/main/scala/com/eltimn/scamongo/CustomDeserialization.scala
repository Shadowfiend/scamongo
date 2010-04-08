package com.eltimn.scamongo {

  import java.lang.reflect.{Field, Type, Method}

  import scala.collection.mutable.Map

  import net.liftweb.json.Formats
  import net.liftweb.json.JsonAST.{JObject, JArray, JField, _}
  import net.liftweb.util.Log

  trait CustomDeserialization[BaseDocument] extends MongoDocumentMeta[BaseDocument] {
    override def create(in: JObject)(implicit formats: Formats): BaseDocument = {
      fromJObject(in)
    }

    def fromJObject(jobject: JObject): BaseDocument

    /**
     * Given a JObject, Class object, and map of field names to setter methods,
     * creates a BaseDocument object and assigns fields from the JObject into the
     * resulting object using the setters in the fieldNameMethodMap.
     *
     * Note that this requires that the clazz object have a default
     * (no-argument) constructor.
     */
    def fromJObject[T](jobject: JObject, clazz: Class[T], fieldNameMethodMap: Map[String, Method]) = {
      val baseInstance = clazz.getDeclaredConstructor().newInstance()

      val objectMap = jobject.values
      objectMap.keys foreach { key =>
        fieldNameMethodMap.get(key) match {
          case Some(fieldSetter) =>
            setFieldWithOptionFallback(baseInstance, fieldSetter,
              convertValueFromJValue((jobject \ key).asInstanceOf[JField].value,
                                     fieldSetter.getParameterTypes()(0)))
          case None => // Don't do anything with this field.
        }
      }

      baseInstance
    }

    /**
     * Converts the given JValue to an Object suitable to be set on an
     * instance. Also provides the expected class, in case the conversion
     * requires that information (e.g., for dates stored as integer timestamps).
     *
     * By default, simply returns lift-json's deserialization.
     */
    protected def convertValueFromJValue(value: JValue, fieldType:Class[_]) = {
      value match {
        case JInt(bigInt) if (fieldType == classOf[Int]) => bigInt.intValue
        case JDouble(double) if (fieldType == classOf[Int]) => double.toInt
        case JDouble(double) if (fieldType == classOf[BigInt]) => BigDecimal(double).toBigInt
        case _ => value.values.asInstanceOf[Object]
      }
    }

    /**
     * Tries to set the field to obj using fieldSetter on instance. If setting
     * the value obj fails, attempts to set it to Some(obj), in case the field
     * takes Options.
     */
    protected def setFieldWithOptionFallback[T](baseInstance: T, fieldSetter: Method, obj: Any) = {
      try {
        setField(baseInstance, fieldSetter, obj)
      } catch {
        // Try to see if we tried to set the value where it needed an option
        // for the value.
        case _:IllegalArgumentException => fieldSetter.invoke(baseInstance, Some(obj))
      }
    }

    /**
     * Tries to set the field to obj using fieldSetter on instance. Subclasses
     * should override this method to handle special types (e.g., Lists).
     */
    protected def setField[T](instance: T, fieldSetter: Method, obj: Any) = {
      fieldSetter.invoke(instance, obj.asInstanceOf[Object])
    }
  }

  trait SimpleDeserialization[BaseDocument] extends CustomDeserialization[BaseDocument] with ClassDataForObject[BaseDocument] {
    def fromJObject(jobject: JObject): BaseDocument = fromJObject(jobject, clazz, fieldNameSetterMap)
  }

  /**
   * Deserializes a JObject to an instance of BaseDocument while resolving any
   * types stored in scamongoType fields. These fields are set in the
   * TypingSerialization trait.
   *
   * Also adds support for deserialization of lists, since their items can be
   * correctly instantiated without manifest information using the typing
   * information embedded with the object.
   */
  trait TypeResolvingDeserialization[BaseDocument] extends SimpleDeserialization[BaseDocument] {
    override def fromJObject(jobject: JObject): BaseDocument = {
      val objectMap = jobject.values

      objectMap.get("scamongoType") match {
        case None => super.fromJObject(jobject)
        case Some(typeName:String) => fromJObject(jobject, typeName).asInstanceOf[BaseDocument]
      }
    }

    def fromJObject(jobject: JObject, typeName: String): Object = {
      val clazz = classForName(typeName)
      fromJObject(jobject, clazz, fieldSetterMapForClass(clazz)).asInstanceOf[Object]
    }

    /**
     * Converts the given JValue to an Object suitable to be set on an
     * instance, including deserializing lists and objects as needed.
     *
     * By default, simply returns lift-json's deserialization.
     */
    override protected def convertValueFromJValue(value: JValue, fieldType:Class[_]) = {
      value match {
        case jarray:JArray   => listFromJArray(jarray)
        case jobject:JObject =>
          jobject.values.get("scamongoType") match {
            case None => fromJObject(jobject, fieldType, fieldSetterMapForClass(fieldType)).asInstanceOf[Object]
            case Some(typeName:String) => fromJObject(jobject, typeName)
          }
        case _ => super.convertValueFromJValue(value, fieldType)
      }
    }

    def listFromJArray(array:JArray) = {
      array.arr.map { value => convertValueFromJValue(value, clazz) }
    }

    def classForName(className:String) = getClass.getClassLoader.loadClass(className)
  }
}
