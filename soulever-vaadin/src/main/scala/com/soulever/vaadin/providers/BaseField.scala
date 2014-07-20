package com.soulever.vaadin.providers

import com.soulever.vaadin.types.{TypeFieldProvider, GeneratedField}
import com.soulever.vaadin.FieldDescriptor
import com.soulever.makro.AbstractFieldDescriptor
import com.vaadin.ui._
import com.vaadin.data.Validator
import scala.util.Try
import com.vaadin.data.Validator.InvalidValueException
import com.vaadin.ui.Button.{ClickEvent, ClickListener}
import com.soulever.makro.Soulever._
import java.util.Date

import scala.util.control.NoStackTrace

trait InlineValidationProvider {

  def inlineValidations:List[(String,String)]

}

trait InlineKeyProvider {

  def inlineKeys:List[(String,String)]

}

trait BaseField[A] extends CustomField[A] with InlineValidationProvider {
  def innerField:AbstractField[_]

  override def validate() = innerField.validate()

  override def focus() = innerField.focus()

  def initContent(): Component = innerField
}

class IntFieldProvider extends TypeFieldProvider[Int, FieldDescriptor] {
  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)(op: Int, baseField: GeneratedField[_,_]): AbstractField[Int] =
    new BaseField[Int] {
      def getType: Class[_ <: Int] = classOf[Int]

      val innerField = new TextField()
      innerField.setValue(op.toString)
      innerField.addValidator(new Validator {
        def validate(value: scala.Any) = if(Try(value.toString.toInt).isFailure) throw new InvalidValueException(inlineValidations.head._1) with NoStackTrace
      })

      override def setValue(newFieldValue: Int) = innerField.setValue(newFieldValue.toString)

      override def getValue: Int = innerField.getValue.toInt

      override def inlineValidations: List[(String,String)] = List("integer" -> "should be an integer")
    }
}

class LongFieldProvider extends TypeFieldProvider[Long, FieldDescriptor] {


  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)(op: Long, baseField: GeneratedField[_,_]): AbstractField[Long] =
    new BaseField[Long] {
      def getType: Class[_ <: Long] = classOf[Long]

      val innerField = new TextField()
      innerField.setValue(op.toString)
      innerField.addValidator(new Validator(){
        def validate(value: scala.Any) = if(Try(value.toString.toLong).isFailure) throw new InvalidValueException(inlineValidations.head._1) with NoStackTrace
      })

      override def setValue(newFieldValue: Long) = innerField.setValue(newFieldValue.toString)

      override def getValue: Long = innerField.getValue.toLong

      override def inlineValidations: List[(String,String)] = List("long" -> "should be a long")
    }
}

class FloatFieldProvider extends TypeFieldProvider[Float, FieldDescriptor] {

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)(op: Float, baseField: GeneratedField[_,_]): AbstractField[Float] =
    new BaseField[Float] {
      def getType: Class[_ <: Float] = classOf[Float]

      val innerField = new TextField()
      innerField.setValue(op.toString)
      innerField.addValidator(new Validator {
        def validate(value: scala.Any) = if(Try(value.toString.toFloat).isFailure) throw new InvalidValueException(inlineValidations.head._1) with NoStackTrace
      })

      override def getValue: Float = innerField.getValue.toFloat

      override def setValue(newFieldValue: Float) = innerField.setValue(newFieldValue.toString)

      override def inlineValidations: List[(String,String)] = List("float" -> "should be a float value")
    }
}

class DoubleFieldProvider extends TypeFieldProvider[Double, FieldDescriptor] {


  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)(op: Double, baseField: GeneratedField[_,_]): AbstractField[Double] =
    new BaseField[Double] {
      def getType: Class[_ <: Double] = classOf[Double]

      val innerField = new TextField()
      innerField.setValue(op.toString)
      innerField.addValidator(new Validator {
        def validate(value: scala.Any) = if(Try(value.toString.toDouble).isFailure) throw new InvalidValueException(inlineValidations.head._1) with NoStackTrace
      })

      override def getValue: Double = innerField.getValue.toDouble

      override def setValue(newFieldValue: Double) = innerField.setValue(newFieldValue.toString)

      override def inlineValidations: List[(String,String)] = List("double" -> "should be a double value")
    }
}

class BooleanFieldProvider extends TypeFieldProvider[Boolean, FieldDescriptor] {

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)(op: Boolean, baseField: GeneratedField[_,_]): AbstractField[Boolean] =
    new CustomField[Boolean] with InlineKeyProvider {
      def getType: Class[_ <: Boolean] = classOf[Boolean]
      val i18nKey = baseField.i18nKey
      private val offLable: String = fieldDescriptor.i18n(i18nKey + "{off}", Some("Off"))
      private val onLable: String = fieldDescriptor.i18n(i18nKey + "{on}", Some("On"))
      var selected = false
      val button: Button = new Button(offLable)
      button.setStyleName("boolean-field-off")
      button.addClickListener(new ClickListener {
        def buttonClick(event: ClickEvent) = setNewValue(!selected)
      })
      setNewValue(op)
      override def setValue(newFieldValue: Boolean) = setNewValue(newFieldValue)

      def setNewValue(newFieldValue: Boolean) {
        selected = newFieldValue
        if (selected) {
          button.setCaption(onLable)
          button.setStyleName("boolean-field-on")
        } else {
          button.setCaption(offLable)
          button.setStyleName("boolean-field-off")
        }
      }

      override def getValue: Boolean = selected
      def initContent(): Component = {
        button
      }

      override def inlineKeys: List[(String,String)] = List("on" -> "On", "off" -> "Off")
    }
}

class ByteFieldProvider extends TypeFieldProvider[Byte, FieldDescriptor]{

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)(op: Byte, baseField: GeneratedField[_,_]): AbstractField[Byte] =
    new BaseField[Byte] {
      def getType: Class[_ <: Byte] = classOf[Byte]

      val innerField = new TextField()
      innerField.setValue((op & 0xFF).toString)
      innerField.addValidator(new Validator {
        def validate(value: scala.Any) =
          if(Try(value.toString.toByte).isFailure) throw new InvalidValueException(inlineValidations.head._1) with NoStackTrace
      })

      override def setValue(newFieldValue: Byte) = innerField.setValue((newFieldValue & 0xFF).toString)

      override def getValue: Byte = innerField.getValue.toByte

      override def inlineValidations: List[(String,String)] = List("byte" -> "should be a byte value")
    }
}

class PasswordFieldProvider extends TypeFieldProvider[Password, FieldDescriptor]{

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)(op: Password, baseField: GeneratedField[_,_]): AbstractField[Password] =
    new BaseField[Password] {

      val innerField = new PasswordField()
      innerField.setValue(op)

      def getType: Class[_ <: Password] = classOf[Password]

      override def getValue: Password = innerField.getValue

      override def setValue(newFieldValue: Password) = innerField.setValue(newFieldValue)

      override def inlineValidations: List[(String, String)] = List.empty
    }
}

class LongTextFieldProvider extends TypeFieldProvider[LongText, FieldDescriptor]{

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)(op: LongText, baseField: GeneratedField[_,_]): AbstractField[LongText] =
    new BaseField[LongText] {
      def getType: Class[_ <: LongText] = classOf[LongText]

      val inf = new TextArea()
      inf.setValue(op)
      def innerField: AbstractField[_] = inf

      override def getValue: LongText = inf.getValue

      override def setValue(newFieldValue: LongText) {
        inf.setValue(newFieldValue)
      }

      override def inlineValidations: List[(String, String)] = List.empty
    }
}

class DateFieldProvider extends TypeFieldProvider[Date, FieldDescriptor]{

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)(op: Date, baseField: GeneratedField[_,_]): AbstractField[Date] = {
    val dateField = new DateField()
    dateField.setValue(op)
    dateField
  }
}