package com.soulever.lift.providers

import java.text.SimpleDateFormat
import java.util.Date

import com.soulever.lift.{FieldDescriptor, helpers}
import com.soulever.lift.types.{TypeFieldProvider, InnerField, GeneratedField}
import com.soulever.makro.{BaseField, AbstractFieldDescriptor}
import com.soulever.makro.Soulever._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.{SetHtml, SetValById, SetElemById}
import net.liftweb.http.{SHtml, LiftRules}

import scala.util.Try
import scala.xml.{NodeSeq, Elem}

/**
 * @Auther tiran 
 * @Date 6/30/14.
 */
class BasicTypedField[A, FD <: AbstractFieldDescriptor[_]](baseField: GeneratedField[_, _],
                                                    op:A,
                                                    empty:A,
                                                    encode:A => String,
                                                    decode:String => A,
                                                    errorMsg:String = "",
                                                    tpe:Option[String] = None) extends InnerField[A] {
  val uniqueId: String = LiftRules.funcNameGenerator()

  var curValue = op.toString

  protected val field:Elem = SHtml.ajaxText(curValue, { s =>
    curValue = s
    val res: Either[String, Any] = baseField.validate
    baseField.toJsCmd(res)
  }, List("id" -> uniqueId) ++ tpe.map("type" -> _).toList:_*)

  def value: A = Try(decode(curValue)).getOrElse(empty)

  override def setValueWithJsCmd(value: A): JsCmd = {
    curValue = value.toString
    SetElemById(uniqueId, curValue, "value")
  }

  override def validate: Either[String, A] = Try(decode(curValue)).toOption.toRight(errorMsg)

  override def innerValidations: List[(String, String)] = List(errorMsg -> errorMsg)

  def elem: NodeSeq = field
}

class StringFieldProvider extends TypeFieldProvider[String, FieldDescriptor] {

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: String, baseField:GeneratedField[_, _]): InnerField[String] =
    new BasicTypedField[String, FD](baseField, op, empty, identity, identity)

  override def empty: String = ""
}

class IntFieldProvider extends TypeFieldProvider[Int, FieldDescriptor] {

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Int, baseField: GeneratedField[_, _]): InnerField[Int] =
    new BasicTypedField[Int, FD](baseField, op, empty, _.toString, _.toInt, "integer")

  override def empty: Int = 0
}

class LongFieldProvider extends TypeFieldProvider[Long, FieldDescriptor] {

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Long, baseField: GeneratedField[_, _]): InnerField[Long] =
    new BasicTypedField[Long, FD](baseField, op, empty, _.toString, _.toLong, "long")

  override def empty: Long = 0
}

class FloatFieldProvider extends TypeFieldProvider[Float, FieldDescriptor] {

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Float, baseField: GeneratedField[_, _]): InnerField[Float] =
    new BasicTypedField[Float, FD](baseField, op, empty, _.toString, _.toFloat, "float")

  override def empty: Float = 0
}

class DoubleFieldProvider extends TypeFieldProvider[Double, FieldDescriptor] {

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Double, baseField: GeneratedField[_, _]): InnerField[Double] =
    new BasicTypedField[Double, FD](baseField, op, empty, _.toString, _.toDouble, "double")

  override def empty: Double = 0
}

class ByteFieldProvider extends TypeFieldProvider[Byte, FieldDescriptor] {

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Byte, baseField: GeneratedField[_, _]): InnerField[Byte] =
    new BasicTypedField[Byte, FD](baseField, op, empty, a => (a & 0xFF).toString, _.toByte, "byte")

  override def empty: Byte = 0
}

class BooleanFieldProvider extends TypeFieldProvider[Boolean, FieldDescriptor] {
  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Boolean, baseField: GeneratedField[_, _]): InnerField[Boolean] =
    new InnerField[Boolean] {
      import helpers.JsCmdHelpers._

      var state = op

      val uniqueId = LiftRules.funcNameGenerator()

      private val offLable: String = fieldDescriptor.i18n(baseField.i18nKey + "{off}", Some("Off"))

      private val onLable: String = fieldDescriptor.i18n(baseField.i18nKey + "{on}", Some("On"))

      override def innerI18nKeys: List[(String, String)] = List("on" -> "On", "off" -> "Off")

      def updateState: JsCmd = {
        val (css, label, oldLabel) = if (state) ("boolean-field-on", onLable, offLable) else ("boolean-field-off", offLable, onLable)
        SetHtml(uniqueId, <span>{label}</span>) & ReplaceClass(uniqueId, oldLabel, label)
      }

      val field = SHtml.ajaxButton(if(state) onLable else offLable, () => {
        state = !state
        updateState
      }, "id" -> uniqueId, "class" -> (if(state) "boolean-field-on" else "boolean-field-off"))

      override def value: Boolean = state

      override def setValueWithJsCmd(value: Boolean): JsCmd = {
        state = value
        updateState
      }

      override def elem: NodeSeq = field

      override def validate: Either[String, Boolean] = Right(true)
    }

  override def empty: Boolean = true
}

class PasswordFieldProvider extends TypeFieldProvider[Password, FieldDescriptor] {

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Password, baseField: GeneratedField[_, _]): InnerField[Password] =
    new BasicTypedField[Password, FD](baseField, op, empty, _.get, Password, tpe = Some("password"))

  override def empty: Password = ""
}

class DateFieldProvider extends TypeFieldProvider[Date, FieldDescriptor] {

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Date, baseField: GeneratedField[_, _]): InnerField[Date] = {

    val format = new SimpleDateFormat("dd/MM/yyyy")

    new BasicTypedField[Date, FD](baseField, op, empty, format.format, format.parse, "date", Some("date"))
  }

  override def empty: Date = new Date()
}

class LongTextFieldProvider extends TypeFieldProvider[LongText, FieldDescriptor] {
  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: LongText, baseField: FieldDescriptor#BaseFieldType[_, _]): InnerField[LongText] =
    new BasicTypedField[LongText, FD](baseField, op, empty, _.value, LongText) {
      override protected val field: Elem = SHtml.ajaxTextarea(curValue, { s =>
        curValue = s
        baseField.toJsCmd(baseField.validate)
      }, "id" -> uniqueId)
    }

  override def empty: LongText = ""
}