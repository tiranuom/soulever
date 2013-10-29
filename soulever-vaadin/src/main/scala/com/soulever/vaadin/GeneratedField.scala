package com.soulever.vaadin

import com.vaadin.ui._
import scala.util.control.Exception._
import com.vaadin.data.Validator.InvalidValueException
import com.soulever.makro.BaseField

class GeneratedField[A : Manifest](init:A,
                                   caption:String,
                                   innerField:AbstractField[A],
                                   validators:List[A => Either[String, A]] = List.empty,
                                   prefix:String = "",
                                   postfix:String = "",
                                   i18n:String => String = identity) extends CustomField[A] with BaseField[A] {
  def getType: Class[_ <: A] = implicitly[Manifest[A]].runtimeClass.asInstanceOf[Class[A]]

  setCaption(i18n(caption))
  setValue(init)

  private def wrapLabel(style:String)(msg:String) = {
    val label: Label = new Label(msg)
    label.setStyleName(style)
    label
  }

  val errorLabel = {
    val label: Label = new Label()
    label.setStyleName("v-field-error")
    label.setVisible(false)
    label
  }

  def initContent(): Component =
    new HorizontalLayout(
      Option(prefix).filterNot(_.isEmpty).map(wrapLabel("v-field-prefix")).toList :::
        List(innerField) :::
        Option(postfix).filterNot(_.isEmpty).map(wrapLabel("v-field-postfix")).toList :::
        List(errorLabel) : _*)

  override def validate() = {
    errorLabel.setVisible(false)
    val result: Either[String, A] = catching(classOf[InvalidValueException]).either {
      innerField.validate()
    }.left.map(_.getMessage).
      right.flatMap {
      _ =>
        val right: Either[String, A] = Right[String, A](getValue)
        (validators foldLeft right) {
          case (e, v) => e.right.flatMap(v)
        }
    }
    result.left.foreach {
      msg =>
        errorLabel.setValue(msg)
        errorLabel.setVisible(true)
    }
  }

  override def isValid: Boolean = {
    errorLabel.setVisible(false)
    val result: Either[String, A] = catching(classOf[InvalidValueException]).either {
      innerField.validate()
    }.left.map(t => s"$caption[${t.getMessage}]").
      right.flatMap {
      _ =>
        val right: Either[String, A] = Right[String, A](getValue)
        (validators foldLeft right) {
          case (e, v) => e.right.flatMap(v)
        }
    }
    result.left.foreach {
      msg =>
        errorLabel.setValue(msg)
        errorLabel.setVisible(true)
    }
    result.isRight
  }

  override def focus() = innerField.focus()

  override def setEnabled(enabled: Boolean) = innerField.setEnabled(enabled)

  override def getValue: A = innerField.getValue

  override def setValue(newFieldValue: A) = innerField.setValue(newFieldValue)

  override def isEnabled: Boolean = innerField.isEnabled
}