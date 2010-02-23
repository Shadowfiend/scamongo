package com.eltimn.scamongo {
  import scala.collection.mutable.{HashMap, Map}

  import java.lang.reflect.{Constructor => JConstructor, Field, Type, Method}

  trait ClassData {
    protected val clazz = Class.forName(
      this.getClass.getName.substring(0, this.getClass.getName.length - 1))
    protected val constructor = clazz.getDeclaredConstructor()
    protected val fieldNameSetterMap = fieldSetterMapForClass(clazz)
    protected val fieldNameGetterMap = fieldGetterMapForClass(clazz)

    /**
     * Returns a map that maps field names to their setter methods in the class.
     */
    def fieldSetterMapForClass(clazz:Class[_]) = {
      new HashMap[String, Method] ++
            (for (method <- clazz.getMethods;
                  methodName = method.getName if methodName.endsWith("_$eq");
                  fieldName = methodName.substring(0, methodName.length - 4) if !fieldName.contains("$"))
              yield (fieldName -> method))
    }

    /**
     * Returns a map that maps field names to their getter methods in the
     * class, but only if they have setter methods.
     */
    def fieldGetterMapForClass(clazz:Class[_]) = {
      new HashMap[String, Method] ++
            (for (method <- clazz.getMethods;
                  methodName = method.getName if methodName.endsWith("_$eq");
                  fieldName = methodName.substring(0, methodName.length - 4) if !fieldName.contains("$"))
              yield (fieldName -> clazz.getMethod(fieldName)))
    }
  }
}
