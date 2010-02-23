package com.eltimn.scamongo {

  import java.lang.reflect.{Constructor => JConstructor, Field, Type, Method}

  import scala.collection.mutable.Map

  import net.liftweb.json.Formats
  import net.liftweb.json.JsonAST.JObject
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
    def fromJObject(jobject: JObject, clazz: Class[BaseDocument], fieldNameMethodMap: Map[String, Method]) = {
      val baseInstance = clazz.getDeclaredConstructor().newInstance().asInstanceOf[BaseDocument]

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

  trait SimpleDeserialization[BaseDocument] extends CustomDeserialization[BaseDocument] with ClassData {
    def fromJObject(jobject: JObject): BaseDocument = {
      val baseInstance = constructor.newInstance().asInstanceOf[BaseDocument]

      val objectMap = jobject.values
      objectMap.keys foreach { key =>
        fieldNameSetterMap.get(key) match {
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

  /**
   * Deserializes a JObject to an instance of BaseDocument while resolving any
   * types stored in scamongoType fields. These fields are set in the
   * TypingSerialization trait.
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

    def classForName(className:String) = getClass.getClassLoader.loadClass(className).asInstanceOf[Class[BaseDocument]]
  }


}
