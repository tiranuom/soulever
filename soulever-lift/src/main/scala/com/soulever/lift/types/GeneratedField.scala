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
                                       innerFieldGenerator: (A, GeneratedField[A, Obj]) => InnerField[A],
                                       validators: List[(A) => Either[String, A]],
                                       secondaryValidators: List[(A, Obj) => Either[String, A]],
                                       css: String,
                                       fieldDescriptor:FieldDescriptor) extends BaseField[A, Obj] with InnerField[A]{
  private val fieldId: String = LiftRules.funcNameGenerator()

  private val errorFieldId = fieldId + "-ERROR"

  def toJsCmd(res:Either[String, _]) =
    Replace(errorFieldId, <span id={errorFieldId} class="soulever-field-error">{res.fold(identity, _ => "")}</span>)

  def updateError(error:String) = toBeEvaluated = Replace(errorFieldId, <span id={errorFieldId} class="soulever-field-error">{error}</span>)

  def clearError = toBeEvaluated = Replace(errorFieldId, <span id={errorFieldId}></span>)

  def updateExpression(cmd:JsCmd) = toBeEvaluated = cmd & clearError

  private var toBeEvaluated:JsCmd = JsCmd.unitToJsCmd()

  def collectUpdate = {
    val update = toBeEvaluated
    toBeEvaluated = JsCmd.unitToJsCmd()
    update
  }

  val i18nKey = caption

  val innerField = innerFieldGenerator(init, this)

  def validate = validators.foldLeft(innerField.validate)(_.right.flatMap(_))

  override def isValid: Boolean = validate.isRight

  override def isValid(obj: Obj): Boolean = {
    val result: Either[String, A] = secondaryValidators.foldLeft(Right(getValue):Either[String, A]){
      case (v, f) => v.right.flatMap(a => f(a, obj))
    }
    result.left.foreach(updateError)
    result.right.foreach(_ => clearError)
    result.isRight
  }

  override def setValue(value: A): Unit = {
    toBeEvaluated = setValueWithJsCmd(value) & Replace(errorFieldId, <span id={errorFieldId}></span>)
  }

  override def getValue: A = innerField.getValue

  override def innerI18nKeys: List[(String, String)] = innerField.innerI18nKeys

  override def innerValidations: List[(String, String)] = innerField.innerValidations

  def elem:NodeSeq = <tr>
    <td>{fieldDescriptor.i18n(caption)}</td>
    <td>{fieldDescriptor.i18n(caption + ".prefix", Some(""))}<span style="margin-right:10px"></span>{innerField.elem}<span style="margin-right:10px"></span>{fieldDescriptor.i18n(caption + ".postfix", Some(""))}</td><td><span id={errorFieldId} class="soulever-field-error"></span></td></tr>

  override def setValueWithJsCmd(value: A): JsCmd = innerField.setValueWithJsCmd(value)
}

trait InnerField[A] {

  def innerI18nKeys: List[(String, String)] = List.empty

  def innerValidations: List[(String, String)] = List.empty

  def getValue: A

  def setValueWithJsCmd(value: A): JsCmd

  def elem: NodeSeq

  def validate:Either[String, A]
}