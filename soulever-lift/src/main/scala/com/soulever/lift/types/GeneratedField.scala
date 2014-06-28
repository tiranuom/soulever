package com.soulever.lift.types

import com.soulever.lift.FieldDescriptor
import com.soulever.makro.BaseField
import net.liftweb.http.js.JsCmd

import scala.xml.NodeSeq

/**
 * @Auther tiran 
 * @Date 6/28/14.
 */
class GeneratedField[A :Manifest, Obj](init: A,
                                       caption: String,
                                       innerFieldGenerator: (Option[A]) => InnerField[A],
                                       validators: List[(A) => Either[String, A]],
                                       secondaryValidators: List[(A, Obj) => Either[String, A]],
                                       css: String) extends BaseField[A, Obj]{

  val innerField = innerFieldGenerator(Option(init))

  override def isValid: Boolean = innerField.isValid

  override def setValue(value: A): Unit = innerField.setValue(value)

  override def getValue: A = innerField.getValue

  override def innerI18nKeys: List[(String, String)] = innerField.innerI18nKeys

  override def isValid(obj: Obj): Boolean = innerField.isValid(obj)

  override def innerValidations: List[(String, String)] = innerField.innerValidations

  def elem:NodeSeq = innerField.elem
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