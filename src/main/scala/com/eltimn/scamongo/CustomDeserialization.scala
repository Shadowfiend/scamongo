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
     *
     * Also correctly converts JArrays to Lists before performing an assignment.
     */
    def fromJObject(jobject: JObject, clazz: Class[BaseDocument], fieldNameMethodMap: Map[String, Method]) = {
      val baseInstance = clazz.getDeclaredConstructor().newInstance().asInstanceOf[BaseDocument]

      val objectMap = jobject.values
      objectMap.keys foreach { key =>
        fieldNameMethodMap.get(key) match {
          case Some(fieldSetter) =>
            setFieldWithOptionFallback(baseInstance, fieldSetter, objectMap(key))
          case None => // Don't do anything with this field.
        }
      }

      baseInstance
    }

    /**
     * Tries to set the field to obj using fieldSetter on instance. If setting
     * the value obj fails, attempts to set it to Some(obj), in case the field
     * takes Options.
     */
    protected def setFieldWithOptionFallback(baseInstance:BaseDocument, fieldSetter:Method, obj:Any) = {
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
    protected def setField(instance:BaseDocument, fieldSetter:Method, obj:Any) = {
      fieldSetter.invoke(instance, obj.asInstanceOf[Object])
    }
  }

  trait SimpleDeserialization[BaseDocument] extends CustomDeserialization[BaseDocument] with ClassDataForObject {
    def fromJObject(jobject: JObject): BaseDocument = {
      val baseInstance = constructor.newInstance().asInstanceOf[BaseDocument]

      val objectMap = jobject.values
      objectMap.keys foreach { key =>
        fieldNameSetterMap.get(key) match {
          case Some(fieldSetter) =>
            try {
              // We want the value to be a JObject or JArray instead of a List
              // or Map when it reaches setField so decisions can be made based on the
              // JSON type.
              //
              // TODO We may want this to always be a JValue and then
              // TODO deserialize similarly to convertValueToJValue in the
              // TODO serializers.
              var value:Any = objectMap(key) match {
                case list:List[_] => (jobject \ key).asInstanceOf[JField].value
                case map:Map[_,_] => (jobject \ key).asInstanceOf[JField].value
                case value        => value
              }
              setField(baseInstance, fieldSetter, value)
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
        case Some(typeName:String) =>
          val clazz = classForName(typeName)

          fromJObject(jobject, clazz, fieldSetterMapForClass(clazz))
      }
    }

    override protected def setField(instance: BaseDocument, fieldSetter: Method, obj: Any) = {
      obj match {
        case array:JArray =>
          fieldSetter.invoke(instance, listFromJArray(array))
        case nonarray =>
          fieldSetter.invoke(instance, nonarray.asInstanceOf[Object])
      }
    }

    def listFromJArray(array:JArray) = {
      array.arr.map { value => fromJObject(value.asInstanceOf[JObject]) }
    }

    def classForName(className:String) = getClass.getClassLoader.loadClass(className).asInstanceOf[Class[BaseDocument]]
  }

}
