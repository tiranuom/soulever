package com.soulever.vaadin

import com.vaadin.ui._
import scala.util.control.Exception._
import com.vaadin.data.Validator.InvalidValueException
import com.soulever.makro.BaseField

class GeneratedField[A : Manifest, Obj](init:A,
                                        caption:String,
                                        innerFieldGenerator:Option[A] => AbstractField[A],
                                        validators:List[A => Either[String, A]] = List.empty,
                                        secondaryValidators:List[(A, Obj) => Either[String, A]] = List.empty,
                                        css:String = "",
                                        prefix:String = "",
                                        postfix:String = "",
                                        i18n:String => String = identity) extends CustomField[A] with BaseField[A, Obj] {
  def getType: Class[_ <: A] = implicitly[Manifest[A]].runtimeClass.asInstanceOf[Class[A]]

  val innerField = innerFieldGenerator(Some(init))

  setCaption(i18n(caption))

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

  def initContent(): Component ={
    val layout: HorizontalLayout = new HorizontalLayout(
      Option(prefix).filterNot(_.isEmpty).map(wrapLabel("v-field-prefix")).toList :::
        List(innerField) :::
        Option(postfix).filterNot(_.isEmpty).map(wrapLabel("v-field-postfix")).toList :::
        List(errorLabel): _*)
    layout.setStyleName(css)
    layout
  }

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

  override def isValid(obj: Obj): Boolean = {
    val right: Either[String, A] = Right[String, A](getValue)
    val result = (secondaryValidators foldLeft right) {
      case (e, f) =>
        e.right.flatMap(a => f(a, obj))
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