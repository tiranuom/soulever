package com.soulever.lift.providers

import java.text.SimpleDateFormat
import java.util.Date

import com.soulever.lift.{FieldDescriptor, helpers}
import com.soulever.lift.types.{TypeFieldProvider, InnerField, GeneratedField}
import com.soulever.makro.MFieldDescriptor
import com.soulever.makro.types.Password
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.SetElemById
import net.liftweb.http.{SHtml, LiftRules}

import scala.util.Try
import scala.xml.{NodeSeq, Elem}

/**
 * @Auther tiran 
 * @Date 6/30/14.
 */
class BasicTypedField[A, FD <: MFieldDescriptor[_]](baseField: GeneratedField[_, _],
                                                    op:Option[A],
                                                    empty:A,
                                                    encode:A => String,
                                                    decode:String => A) extends InnerField[A] {
  val uniqueId: String = LiftRules.funcNameGenerator()
  var curValue = op.getOrElse(empty).toString
  var tempValue = empty.toString
  private val text:Elem = SHtml.ajaxText(curValue, { s =>
    tempValue = s
    if (baseField.isValid) curValue = s
    baseField.toBeEvaluated
  }, "id" -> uniqueId)
  def getValue: A = Try(decode(curValue)).getOrElse(empty)
  def setValue(value: A): Unit = {
    curValue = value.toString
    baseField.updateExpression(SetElemById(uniqueId, curValue, "value"))
  }
  def elem: NodeSeq = text
}

class StringFieldProvider extends TypeFieldProvider[String, FieldDescriptor] {

  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Option[String], baseField:GeneratedField[_, _]): InnerField[String] =
    new BasicTypedField[String, FD](baseField, op, empty, identity, identity)

  override def empty: String = ""
}

class IntFieldProvider extends TypeFieldProvider[Int, FieldDescriptor] {

  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Option[Int], baseField: GeneratedField[_, _]): InnerField[Int] =
    new BasicTypedField[Int, FD](baseField, op, empty, _.toString, _.toInt) {
      override def isValid: Boolean = {
        val t = Try(tempValue.toInt).isSuccess
        if(t) baseField.clearError else baseField.updateError("integer")
        t
      }

      override def innerValidations: List[(String, String)] = List("integer" -> "integer")
    }

  override def empty: Int = 0
}

class LongFieldProvider extends TypeFieldProvider[Long, FieldDescriptor] {

  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Option[Long], baseField: GeneratedField[_, _]): InnerField[Long] =
    new BasicTypedField[Long, FD](baseField, op, empty, _.toString, _.toLong) {
      override def isValid: Boolean = {
        val t = Try(tempValue.toInt).isSuccess
        if(t) baseField.clearError else baseField.updateError("long")
        t
      }

      override def innerValidations: List[(String, String)] = List("long" -> "long")
    }

  override def empty: Long = 0
}

class FloatFieldProvider extends TypeFieldProvider[Float, FieldDescriptor] {

  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Option[Float], baseField: GeneratedField[_, _]): InnerField[Float] =
    new BasicTypedField[Float, FD](baseField, op, empty, _.toString, _.toFloat) {
      override def isValid: Boolean = {
        val t = Try(tempValue.toInt).isSuccess
        if(t) baseField.clearError else baseField.updateError("float")
        t
      }

      override def innerValidations: List[(String, String)] = List("float" -> "float")
    }

  override def empty: Float = 0
}

class DoubleFieldProvider extends TypeFieldProvider[Double, FieldDescriptor] {

  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Option[Double], baseField: GeneratedField[_, _]): InnerField[Double] =
    new BasicTypedField[Double, FD](baseField, op, empty, _.toString, _.toDouble) {
      override def isValid: Boolean = {
        val t = Try(tempValue.toInt).isSuccess
        if(t) baseField.clearError else baseField.updateError("double")
        t
      }

      override def innerValidations: List[(String, String)] = List("double" -> "double")
    }

  override def empty: Double = 0
}

class BooleanFieldProvider extends TypeFieldProvider[Boolean, FieldDescriptor] {
  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Option[Boolean], baseField: GeneratedField[_, _]): InnerField[Boolean] = new InnerField[Boolean] {
    import helpers.JsCmdHelpers._
    var state = op.getOrElse(empty)
    val uniqueId = LiftRules.funcNameGenerator()

    private val offLable: String = fieldDescriptor.i18n(baseField.i18nKey + "{off}")
    private val onLable: String = fieldDescriptor.i18n(baseField.i18nKey + "{on}")

    override def innerI18nKeys: List[(String, String)] = List("on" -> "On", "off" -> "Off")

    def updateState: JsCmd = {
      val (css, label, oldLabel) = if (state) ("boolean-field-on", onLable, offLable) else ("boolean-field-off", offLable, onLable)
      SetElemById(uniqueId, css, "className") & ReplaceClass(uniqueId, oldLabel, label)
    }

    val field = SHtml.ajaxButton(if(state) onLable else offLable, () => {
      state = !state
      updateState
    }, "id" -> uniqueId)

    override def getValue: Boolean = state

    override def setValue(value: Boolean): Unit = {
      state = value
      baseField.updateExpression(updateState)
    }

    override def elem: NodeSeq = field
  }

  override def empty: Boolean = true
}

class PasswordFieldProvider extends TypeFieldProvider[Password, FieldDescriptor] {

  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Option[Password], baseField: GeneratedField[_, _]): InnerField[Password] = new InnerField[Password] {

    val uniqueId: String = LiftRules.funcNameGenerator()
    var curValue = op.getOrElse(empty).toString
    var tempValue = empty.toString
    private val text:Elem = SHtml.ajaxText(curValue, { s =>
      tempValue = s
      if (baseField.isValid) curValue = s
      baseField.toBeEvaluated
    }, "type" -> "password", "id" -> uniqueId) // hack and security breaches. check proper way of doing this.

    def getValue: Password = curValue
    def setValue(value: Password): Unit ={
      baseField.updateExpression(baseField.updateExpression(SetElemById(uniqueId, curValue, "value")))
      curValue = value
    }
    def elem: NodeSeq = text
  }

  override def empty: Password = ""
}

class ByteFieldProvider extends TypeFieldProvider[Byte, FieldDescriptor] {

  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Option[Byte], baseField: GeneratedField[_, _]): InnerField[Byte] =
    new BasicTypedField[Byte, FD](baseField, op, empty, a => (a & 0xFF).toString, _.toByte) {
      override def isValid: Boolean = {
        val t = Try(tempValue.toInt).isSuccess
        if(t) baseField.clearError else baseField.updateError("byte")
        t
      }

      override def innerValidations: List[(String, String)] = List("byte" -> "byte")
    }

  override def empty: Byte = 0
}

class DateFieldProvider extends TypeFieldProvider[Date, FieldDescriptor] {

  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Option[Date], baseField: GeneratedField[_, _]): InnerField[Date] = new InnerField[Date] {

    val format = new SimpleDateFormat("dd/MM/yyyy")
    var curValue = op.getOrElse(empty).toString
    var tempValue = empty.toString
    private val field: Elem = SHtml.ajaxText(curValue, { s =>
      tempValue = s
      if (baseField.isValid) curValue = s
      baseField.toBeEvaluated
    }, "type" -> "date") // hack and security breaches. check proper way of doing this.

    def getValue: Date = Try(format.parse(curValue)).getOrElse(new Date())

    def setValue(value: Date): Unit = curValue = format.format(value)

    override def isValid: Boolean = {
      val t: Boolean = Try(format.parse(curValue)).isSuccess
      if(t) baseField.clearError else baseField.updateError("date")
      t
    }

    override def elem: NodeSeq = field
  }
  override def empty: Date = new Date()
}
