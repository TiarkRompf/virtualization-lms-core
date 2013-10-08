package scala.virtualization.lms
package util

/**
 * An object for reflection related utility methods
 */
object ReflectionUtil {
  /**
   * This method accepts an instance of a case class and returns 
   * the list of its fields.
   * Each entry in the returned list is a tripple:
   *  - field name
   *  - field type
   *  - field value
   */
  def caseNameTypeValues(a: AnyRef) = {
  	  /**
  	   * returns number of parameters for the first constructor of an object
  	   */
	  def numConstructorParams(a: AnyRef) = a.getClass.getConstructors()(0).getParameterTypes.size
	  /**
	   * returns list of fields in an instance of a case class
	   */
	  def caseFields(a: AnyRef) = a.getClass.getDeclaredFields.toSeq.filterNot(_.isSynthetic).take(numConstructorParams(a)).map{field =>
	          field.setAccessible(true)
	          field
	  }
  	caseFields(a).map{field => (field.getName, field.getType, field.get(a))}
  }
}