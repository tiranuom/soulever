package com.soulever.vaadin.providers

import com.soulever.vaadin.TypeFieldProvider
import com.soulever.makro.MFieldDescriptor
import com.vaadin.ui._
import com.vaadin.data.Validator
import scala.util.Try
import com.vaadin.data.Validator.InvalidValueException
import com.vaadin.ui.Button.{ClickEvent, ClickListener}
import com.soulever.makro.types.{LongText, Password}
import java.util.Date

trait BaseField[A] extends CustomField[A]{
  def innerField:AbstractField[_]

  override def validate() = innerField.validate()

  override def focus() = innerField.focus()

  def initContent(): Component = innerField
}

class IntFieldProvider extends TypeFieldProvider[Int] {
  def field[FD <: MFieldDescriptor[_]](implicit fieldDescriptor: FD): (Option[Int]) => AbstractField[Int] = {
    op =>
      new BaseField[Int] {
        def getType: Class[_ <: Int] = classOf[Int]

        val innerField = new TextField()
        op.map(_.toString).foreach(innerField.setValue)
        innerField.addValidator(new Validator {
          def validate(value: scala.Any) = if(Try(value.toString.toInt).isFailure) throw new InvalidValueException("integer")
        })

        override def setValue(newFieldValue: Int) = innerField.setValue(newFieldValue.toString)

        override def getValue: Int = innerField.getValue.toInt
      }
  }
}

class LongFieldProvider extends TypeFieldProvider[Long] {
  def field[FD <: MFieldDescriptor[_]](implicit fieldDescriptor: FD): (Option[Long]) => AbstractField[Long] = {
    op =>
      new BaseField[Long] {
        def getType: Class[_ <: Long] = classOf[Long]

        val innerField = new TextField()
        op.map(_.toString).foreach(innerField.setValue)
        innerField.addValidator(new Validator(){
          def validate(value: scala.Any) = if(Try(value.toString.toLong).isFailure) throw new InvalidValueException("long")
        })

        override def setValue(newFieldValue: Long) = innerField.setValue(newFieldValue.toString)

        override def getValue: Long = innerField.getValue.toLong
      }
  }
}

class FloatFieldProvider extends TypeFieldProvider[Float] {
  def field[FD <: MFieldDescriptor[_]](implicit fieldDescriptor: FD): (Option[Float]) => AbstractField[Float] = {
    op =>
      new BaseField[Float] {
        def getType: Class[_ <: Float] = classOf[Float]

        val innerField = new TextField()
        op.map(_.toString).foreach(innerField.setValue)
        innerField.addValidator(new Validator {
          def validate(value: scala.Any) = if(Try(value.toString.toFloat).isFailure) throw new InvalidValueException("float")
        })

        override def getValue: Float = innerField.getValue.toFloat

        override def setValue(newFieldValue: Float) = innerField.setValue(newFieldValue.toString)
      }
  }
}

class DoubleFieldProvider extends TypeFieldProvider[Double] {
  def field[FD <: MFieldDescriptor[_]](implicit fieldDescriptor: FD): (Option[Double]) => AbstractField[Double] = {
    op =>
      new BaseField[Double] {
        def getType: Class[_ <: Double] = classOf[Double]

        val innerField = new TextField()
        op.map(_.toString).foreach(innerField.setValue)
        innerField.addValidator(new Validator {
          def validate(value: scala.Any) = if(Try(value.toString.toDouble).isFailure) throw new InvalidValueException("double")
        })

        override def getValue: Double = innerField.getValue.toDouble

        override def setValue(newFieldValue: Double) = innerField.setValue(newFieldValue.toString)
      }
  }
}

class BooleanFieldProvider extends TypeFieldProvider[Boolean] {
  def field[FD <: MFieldDescriptor[_]](implicit fieldDescriptor: FD): (Option[Boolean]) => AbstractField[Boolean] = {
    op =>
      new CustomField[Boolean] {
        def getType: Class[_ <: Boolean] = classOf[Boolean]

        var selected = false
        val button: Button = new Button("off")
        button.setStyleName("boolean-field-off")
        button.addClickListener(new ClickListener {
          def buttonClick(event: ClickEvent) = setNewValue(!selected)
        })
        op.foreach(setNewValue)
        override def setValue(newFieldValue: Boolean) = setNewValue(newFieldValue)

        def setNewValue(newFieldValue: Boolean) {
          selected = newFieldValue
          if (selected) {
            button.setCaption("on")
            button.setStyleName("boolean-field-on")
          } else {
            button.setCaption("off")
            button.setStyleName("boolean-field-off")
          }
        }

        override def getValue: Boolean = selected
        def initContent(): Component = {
          button
        }
      }
  }
}

class ByteFieldProvider extends TypeFieldProvider[Byte]{
  def field[FD <: MFieldDescriptor[_]](implicit fieldDescriptor: FD): (Option[Byte]) => AbstractField[Byte] = {
    op =>
      new BaseField[Byte] {
        def getType: Class[_ <: Byte] = classOf[Byte]

        val innerField = new TextField()
        op.map(b => (b & 0xFF).toString).foreach(innerField.setValue)
        innerField.addValidator(new Validator {
          def validate(value: scala.Any) =
            if(Try(value.toString.toByte).isFailure) throw new InvalidValueException("byte")
        })

        override def setValue(newFieldValue: Byte) = innerField.setValue((newFieldValue & 0xFF).toString)

        override def getValue: Byte = innerField.getValue.toByte
      }
  }
}

class PasswordFieldProvider extends TypeFieldProvider[Password]{
  def field[FD <: MFieldDescriptor[_]](implicit fieldDescriptor: FD): (Option[Password]) => AbstractField[Password] = {
    op =>
      new BaseField[Password] {

        val innerField = new PasswordField()
        op.foreach(s => innerField.setValue(s))

        def getType: Class[_ <: Password] = classOf[Password]

        override def getValue: Password = innerField.getValue

        override def setValue(newFieldValue: Password) = innerField.setValue(newFieldValue)
      }
  }
}

class LongTextFieldProvider extends TypeFieldProvider[LongText]{
  def field[FD <: MFieldDescriptor[_]](implicit fieldDescriptor: FD): (Option[LongText]) => AbstractField[LongText] = {
    value =>
      new BaseField[LongText] {
        def getType: Class[_ <: LongText] = classOf[LongText]

        val inf = new TextArea()
        value.foreach(a => inf.setValue(a))
        def innerField: AbstractField[_] = inf

        override def getValue: LongText = inf.getValue

        override def setValue(newFieldValue: LongText) {
          inf.setValue(newFieldValue)
        }
      }
  }
}

class DateFieldProvider extends TypeFieldProvider[Date]{
  def field[FD <: MFieldDescriptor[_]](implicit fieldDescriptor: FD): (Option[Date]) => AbstractField[Date] = {
    value =>
      val dateField = new DateField()
      value.foreach(dateField.setValue)
      dateField
  }
}