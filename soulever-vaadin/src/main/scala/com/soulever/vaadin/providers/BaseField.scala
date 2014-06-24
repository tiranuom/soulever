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

trait InlineValidationProvider {

  def inlineValidations:List[String]

}

trait InlineKeyProvider {

  def inlineKeys:List[String]

}

trait BaseField[A] extends CustomField[A] with InlineValidationProvider{
  def innerField:AbstractField[_]

  override def validate() = innerField.validate()

  override def focus() = innerField.focus()

  def initContent(): Component = innerField
}

class IntFieldProvider extends TypeFieldProvider[Int] {
  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD, i18nKey:String)(op: Option[Int]): AbstractField[Int] =
    new BaseField[Int] {
      def getType: Class[_ <: Int] = classOf[Int]

      val innerField = new TextField()
      op.map(_.toString).foreach(innerField.setValue)
      innerField.addValidator(new Validator {
        def validate(value: scala.Any) = if(Try(value.toString.toInt).isFailure) throw new InvalidValueException(inlineValidations.head)
      })

      override def setValue(newFieldValue: Int) = innerField.setValue(newFieldValue.toString)

      override def getValue: Int = innerField.getValue.toInt

      override def inlineValidations: List[String] = List("integer")
    }


  override def empty: Int = 0
}

class LongFieldProvider extends TypeFieldProvider[Long] {


  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD, i18nKey:String)(op: Option[Long]): AbstractField[Long] =
    new BaseField[Long] {
      def getType: Class[_ <: Long] = classOf[Long]

      val innerField = new TextField()
      op.map(_.toString).foreach(innerField.setValue)
      innerField.addValidator(new Validator(){
        def validate(value: scala.Any) = if(Try(value.toString.toLong).isFailure) throw new InvalidValueException(inlineValidations.head)
      })

      override def setValue(newFieldValue: Long) = innerField.setValue(newFieldValue.toString)

      override def getValue: Long = innerField.getValue.toLong

      override def inlineValidations: List[String] = List("long")
    }


  override def empty: Long = 0
}

class FloatFieldProvider extends TypeFieldProvider[Float] {

  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD, i18nKey:String)(op: Option[Float]): AbstractField[Float] =
    new BaseField[Float] {
      def getType: Class[_ <: Float] = classOf[Float]

      val innerField = new TextField()
      op.map(_.toString).foreach(innerField.setValue)
      innerField.addValidator(new Validator {
        def validate(value: scala.Any) = if(Try(value.toString.toFloat).isFailure) throw new InvalidValueException(inlineValidations.head)
      })

      override def getValue: Float = innerField.getValue.toFloat

      override def setValue(newFieldValue: Float) = innerField.setValue(newFieldValue.toString)

      override def inlineValidations: List[String] = List("float")
    }


  override def empty: Float = 0
}

class DoubleFieldProvider extends TypeFieldProvider[Double] {


  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD, i18nKey:String)(op: Option[Double]): AbstractField[Double] =
    new BaseField[Double] {
      def getType: Class[_ <: Double] = classOf[Double]

      val innerField = new TextField()
      op.map(_.toString).foreach(innerField.setValue)
      innerField.addValidator(new Validator {
        def validate(value: scala.Any) = if(Try(value.toString.toDouble).isFailure) throw new InvalidValueException(inlineValidations.head)
      })

      override def getValue: Double = innerField.getValue.toDouble

      override def setValue(newFieldValue: Double) = innerField.setValue(newFieldValue.toString)

      override def inlineValidations: List[String] = List("double")
    }


  override def empty: Double = 0
}

class BooleanFieldProvider extends TypeFieldProvider[Boolean] {

  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD, i18nKey:String)(op: Option[Boolean]): AbstractField[Boolean] =
    new CustomField[Boolean] with InlineKeyProvider {
      def getType: Class[_ <: Boolean] = classOf[Boolean]
      private val offLable: String = fieldDescriptor.i18n(i18nKey + "{off}")
      private val onLable: String = fieldDescriptor.i18n(i18nKey + "{on}")
      var selected = false
      val button: Button = new Button(offLable)
      button.setStyleName("boolean-field-off")
      button.addClickListener(new ClickListener {
        def buttonClick(event: ClickEvent) = setNewValue(!selected)
      })
      op.foreach(setNewValue)
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

      override def inlineKeys: List[String] = List("on", "off")
    }


  override def empty: Boolean = false
}

class ByteFieldProvider extends TypeFieldProvider[Byte]{

  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD, i18nKey:String)(op: Option[Byte]): AbstractField[Byte] =
    new BaseField[Byte] {
      def getType: Class[_ <: Byte] = classOf[Byte]

      val innerField = new TextField()
      op.map(b => (b & 0xFF).toString).foreach(innerField.setValue)
      innerField.addValidator(new Validator {
        def validate(value: scala.Any) =
          if(Try(value.toString.toByte).isFailure) throw new InvalidValueException(inlineValidations.head)
      })

      override def setValue(newFieldValue: Byte) = innerField.setValue((newFieldValue & 0xFF).toString)

      override def getValue: Byte = innerField.getValue.toByte

      override def inlineValidations: List[String] = List("byte")
    }

  override def empty: Byte = 0
}

class PasswordFieldProvider extends TypeFieldProvider[Password]{

  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD, i18nKey:String)(op: Option[Password]): AbstractField[Password] =
    new BaseField[Password] {

      val innerField = new PasswordField()
      op.foreach(s => innerField.setValue(s))

      def getType: Class[_ <: Password] = classOf[Password]

      override def getValue: Password = innerField.getValue

      override def setValue(newFieldValue: Password) = innerField.setValue(newFieldValue)

      override def inlineValidations: List[String] = List.empty
    }

  override def empty: Password = new Password("")
}

class LongTextFieldProvider extends TypeFieldProvider[LongText]{

  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD, i18nKey:String)(op: Option[LongText]): AbstractField[LongText] =
    new BaseField[LongText] {
      def getType: Class[_ <: LongText] = classOf[LongText]

      val inf = new TextArea()
      op.foreach(a => inf.setValue(a))
      def innerField: AbstractField[_] = inf

      override def getValue: LongText = inf.getValue

      override def setValue(newFieldValue: LongText) {
        inf.setValue(newFieldValue)
      }

      override def inlineValidations: List[String] = List.empty
    }

  override def empty: LongText = new LongText("")
}

class DateFieldProvider extends TypeFieldProvider[Date]{

  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD, i18nKey:String)(op: Option[Date]): AbstractField[Date] = {
    val dateField = new DateField()
    op.foreach(dateField.setValue)
    dateField
  }

  override def empty: Date = new Date()
}