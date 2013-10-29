package com.soulever.vaadin.providers

import com.soulever.vaadin.TypeFieldProvider
import com.soulever.makro.MFieldDescriptor
import com.vaadin.ui._
import com.vaadin.data.Validator
import scala.util.Try
import com.vaadin.data.Validator.InvalidValueException

trait BaseField[A] extends CustomField[A]{
  def innerField:AbstractField[_]

  override def validate() = innerField.validate()

  override def focus() = innerField.focus()

  def initContent(): Component = innerField
}

class IntFieldProvider extends TypeFieldProvider[Int] {
  def field[FD <: MFieldDescriptor[_]](implicit fieldDescriptor: FD): AbstractField[Int] = new BaseField[Int] {
    def getType: Class[_ <: Int] = classOf[Int]

    val innerField = new TextField()
    innerField.addValidator(new Validator {
      def validate(value: scala.Any) = if(Try(value.toString.toInt).isFailure) throw new InvalidValueException("integer")
    })

    override def setValue(newFieldValue: Int) = innerField.setValue(newFieldValue.toString)

    override def getValue: Int = innerField.getValue.toInt
  }
}

class LongFieldProvider extends TypeFieldProvider[Long] {
  def field[FD <: MFieldDescriptor[_]](implicit fieldDescriptor: FD): AbstractField[Long] = new BaseField[Long] {
    def getType: Class[_ <: Long] = classOf[Long]

    val innerField = new TextField()
    innerField.addValidator(new Validator(){
      def validate(value: scala.Any) = if(Try(value.toString.toLong).isFailure) throw new InvalidValueException("long")
    })

    override def setValue(newFieldValue: Long) = innerField.setValue(newFieldValue.toString)

    override def getValue: Long = innerField.getValue.toLong
  }
}

class FloatFieldProvider extends TypeFieldProvider[Float] {
  def field[FD <: MFieldDescriptor[_]](implicit fieldDescriptor: FD): AbstractField[Float] = new BaseField[Float] {
    def getType: Class[_ <: Float] = classOf[Float]

    val innerField = new TextField()
    innerField.addValidator(new Validator {
      def validate(value: scala.Any) = if(Try(value.toString.toFloat).isFailure) throw new InvalidValueException("float")
    })

    override def getValue: Float = innerField.getValue.toFloat

    override def setValue(newFieldValue: Float) = innerField.setValue(newFieldValue.toString)
  }
}

class DoubleFieldProvider extends TypeFieldProvider[Double] {
  def field[FD <: MFieldDescriptor[_]](implicit fieldDescriptor: FD): AbstractField[Double] = new BaseField[Double] {
    def getType: Class[_ <: Double] = classOf[Double]

    val innerField = new TextField()
    innerField.addValidator(new Validator {
      def validate(value: scala.Any) = if(Try(value.toString.toDouble).isFailure) throw new InvalidValueException("double")
    })

    override def getValue: Double = innerField.getValue.toDouble

    override def setValue(newFieldValue: Double) = innerField.setValue(newFieldValue.toString)
  }
}

class BooleanFieldProvider extends TypeFieldProvider[Boolean] {
  def field[FD <: MFieldDescriptor[_]](implicit fieldDescriptor: FD): AbstractField[Boolean] = new BaseField[Boolean] {
    def getType: Class[_ <: Boolean] = classOf[Boolean]

    val innerField: CheckBox = new CheckBox()

    override def setValue(newFieldValue: Boolean) = innerField.setValue(newFieldValue)

    override def getValue: Boolean = innerField.getValue
  }
}

class ByteFieldProvider extends TypeFieldProvider[Byte]{
  def field[FD <: MFieldDescriptor[_]](implicit fieldDescriptor: FD): AbstractField[Byte] =
    new BaseField[Byte] {
      def getType: Class[_ <: Byte] = classOf[Byte]

      val innerField = new TextField()
      innerField.addValidator(new Validator {
        def validate(value: scala.Any) =
          if(Try(value.toString.toByte).isFailure) throw new InvalidValueException("byte")
      })

      override def setValue(newFieldValue: Byte) = innerField.setValue((newFieldValue & 0xFF).toString)

      override def getValue: Byte = innerField.getValue.toByte
    }
}