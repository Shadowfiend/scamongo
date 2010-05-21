package com.eltimn.scamongo {

import net.liftweb.json.JsonAST.JObject

import com.mongodb._

trait DocumentError {
  def message : String
}
object DocumentError {
  implicit def tupleToFieldError(tuple:(String, String)) : DocumentError = tuple match {
    case (field, message) => FieldError(field, message)
  }
  implicit def stringToGlobalError(string:String) : DocumentError = GlobalError(string)
}

case class GlobalError(message: String) extends DocumentError
case class FieldError(field: String, message: String) extends DocumentError

trait Validation {
  def validations:List[()=>Option[DocumentError]] = List()

  def validate : List[DocumentError] = {
    validations.foldLeft(List[DocumentError]())( (errors, validation) => {
        validation() match {
          case Some(error) => error :: errors
          case None        => errors
        }
      }
    )
  }
}

}
