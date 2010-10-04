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
      val methods = clazz.getMethods // So we can omit getters without setters.
      new HashMap[String, Method] ++
            (for (method <- methods;
                  methodName = method.getName if methodName.endsWith("_$eq");
                  fieldName = methodName.substring(0, methodName.length - 4) if !fieldName.contains("$") &&
                                                                                methods.exists { m => m.getName == fieldName };
                  getter = clazz.getMethod(fieldName))
              yield (fieldName -> getter))
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
   * Provides a singleton place to store the class data for objects. This
   * allows us to only keep one copy of the field name setter/getter maps,
   * allowing us to minimize the number of Method objects that are kept around.
   */
  object ObjectClassInfoCache extends ClassDataExtraction {
    class ClassInfo(className:String) {
      private val cachedClazz = Class.forName(className)
      def clazz[T] : Class[T] = cachedClazz.asInstanceOf[Class[T]]

      val constructor = cachedClazz.getDeclaredConstructor()
      val fieldNameSetterMap = fieldSetterMapForClass(cachedClazz)
      val fieldNameGetterMap = fieldGetterMapForClass(cachedClazz)
    }

    private var cache:Map[String, ClassInfo] = new HashMap[String, ClassInfo]

    def classInfoFor(className:String) : ClassInfo = {
      (for (result <- cache.get(className)) yield result) getOrElse {
        val info = new ClassInfo(className)

        cache = cache + (className -> info)

        info
      }
    }
  }

  /**
   * Mix in this trait for the same effect as ClassData. The key difference is
   * that this should be used in (singleton) objects instead of classes. The
   * key here is a manipulation done to determine the clazz object that is
   * inferred. For singleton objects, the name that is derived is different.
   */
  trait ClassDataForObject[T] extends ClassDataExtraction {
    /**
     * We do not want to singleton object's class here, so we look up its
     * companion class by name, using the fact that the singleton is named
     * Class$, where Class is the name of the companion class.
     */
    protected val classInfo:ObjectClassInfoCache.ClassInfo =
      ObjectClassInfoCache.classInfoFor(this.getClass.getName.substring(0, this.getClass.getName.length - 1))
    protected val clazz:Class[T] = classInfo.clazz
    protected val constructor = classInfo.constructor
    protected val fieldNameSetterMap = classInfo.fieldNameSetterMap
    protected val fieldNameGetterMap = classInfo.fieldNameGetterMap
  }
}
