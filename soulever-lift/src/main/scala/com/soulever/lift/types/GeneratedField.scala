package com.soulever.lift.types

import com.soulever.lift.FieldDescriptor
import com.soulever.makro.BaseField
import net.liftweb.http.LiftRules
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.Replace

import scala.util.Try
import scala.xml.NodeSeq

/**
 * @Auther tiran 
 * @Date 6/28/14.
 */
class GeneratedField[A :Manifest, Obj](init: A,
                                       caption: String,
                                       innerFieldGenerator: (Option[A], GeneratedField[A, Obj]) => InnerField[A],
                                       validators: List[(A) => Either[String, A]],
                                       secondaryValidators: List[(A, Obj) => Either[String, A]],
                                       css: String,
                                       i18n:String => String) extends BaseField[A, Obj]{
  private val fieldId: String = LiftRules.funcNameGenerator()

  private val errorFieldId = fieldId + "-ERROR"

  def updateError(error:String) = toBeEvaluated = Replace(errorFieldId, <span id={errorFieldId} class="soulever-field-error">{error}</span>)

  def clearError = toBeEvaluated = Replace(errorFieldId, <span id={errorFieldId}></span>)

  def updateExpression(cmd:JsCmd) = toBeEvaluated = cmd & clearError

  var toBeEvaluated:JsCmd = JsCmd.unitToJsCmd()

  val i18nKey = caption

  val innerField = innerFieldGenerator(Option(init), this)

  override def isValid: Boolean = {
    if (innerField.isValid) {
      val result: Either[String, A] = validators.foldLeft(Right(getValue):Either[String, A])(_.right.flatMap(_))
      result.left.foreach(updateError)
      result.right.foreach(_ => clearError)
      result.isRight
    } else false
  }

  override def isValid(obj: Obj): Boolean = innerField.isValid(obj) && {
    val result: Either[String, A] = secondaryValidators.foldLeft(Right(getValue):Either[String, A]){
      case (v, f) => v.right.flatMap(a => f(a, obj))
    }
    result.left.foreach(updateError)
    result.right.foreach(_ => clearError)
    result.isRight
  }

  override def setValue(value: A): Unit = innerField.setValue(value)

  override def getValue: A = innerField.getValue

  override def innerI18nKeys: List[(String, String)] = innerField.innerI18nKeys

  override def innerValidations: List[(String, String)] = innerField.innerValidations

  def elem:NodeSeq = <tr><td>{i18n(caption)}</td><td>{innerField.elem}</td><td><span id={errorFieldId} class="soulever-field-error"></span></td></tr>
}

trait InnerField[A] {

  var jsUpdate = JsCmd.unitToJsCmd()

  def isValid: Boolean = true

  def isValid(obj:Any): Boolean = true

  def innerI18nKeys: List[(String, String)] = List.empty

  def innerValidations: List[(String, String)] = List.empty

  def getValue: A

  def setValue(value: A): Unit

  def elem: NodeSeq

  def updateJs = jsUpdate

}