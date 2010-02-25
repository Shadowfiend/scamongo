package com.eltimn.scamongo {
  import scala.collection.mutable.{HashMap, Map}

  import java.lang.reflect.{Constructor => JConstructor, Field, Type, Method}

  /**
   * Provides methods for acquiring maps of field names to setter and getter
   * methods for arbitrary classes.
   */
  trait ClassDataExtraction {
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

  /**
   * Mix in this trait to get a set of field name to setter and field name to
   * getter maps, as well as  available clazz and constructor vals with the
   * appropriate Class and Method objects in them.
   *
   * Additionally, this trait provides a method to fetch a field to field
   * setter method map for an arbitrary class, as well as the same map for
   * field getters.
   */
  trait ClassData extends ClassDataExtraction {
    protected val clazz = this.getClass
    protected val constructor = clazz.getDeclaredConstructor()
    protected val fieldNameSetterMap = fieldSetterMapForClass(clazz)
    protected val fieldNameGetterMap = fieldGetterMapForClass(clazz)
  }

  /**
   * Mix in this trait for the same effect as ClassData. The key difference is
   * that this should be used in objects instead of classes. The key here is a
   * manipulation done to determine the clazz object that is inferred. For
   * objects, the name that is derived is different.
   */
  trait ClassDataForObject extends ClassDataExtraction {
    protected val clazz = Class.forName(
      this.getClass.getName.substring(0, this.getClass.getName.length - 1))
    protected val constructor = clazz.getDeclaredConstructor()
    protected val fieldNameSetterMap = fieldSetterMapForClass(clazz)
    protected val fieldNameGetterMap = fieldGetterMapForClass(clazz)
  }
}
