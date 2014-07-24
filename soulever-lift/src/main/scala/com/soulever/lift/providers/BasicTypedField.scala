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

  def value: A = Try(decode(curValue)).getOrElse(op)

  override def setValueWithJsCmd(value: A): JsCmd = {
    curValue = value.toString
    SetElemById(uniqueId, curValue, "value")
  }

  override def validate: Either[String, A] = Try(decode(curValue)).toOption.toRight(errorMsg)

  override def innerValidations: List[(String, String)] = List(errorMsg -> errorMsg)

  def elem: NodeSeq = field
}

class StringFieldProvider extends TypeFieldProvider[String] {

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: String, baseField:GeneratedField[_, _]): InnerField[String] =
    new BasicTypedField[String, FD](baseField, op, identity, identity)
}

class IntFieldProvider extends TypeFieldProvider[Int] {

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Int, baseField: GeneratedField[_, _]): InnerField[Int] =
    new BasicTypedField[Int, FD](baseField, op, _.toString, _.toInt, "integer")
}

class LongFieldProvider extends TypeFieldProvider[Long] {

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Long, baseField: GeneratedField[_, _]): InnerField[Long] =
    new BasicTypedField[Long, FD](baseField, op, _.toString, _.toLong, "long")
}

class FloatFieldProvider extends TypeFieldProvider[Float] {

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Float, baseField: GeneratedField[_, _]): InnerField[Float] =
    new BasicTypedField[Float, FD](baseField, op, _.toString, _.toFloat, "float")
}

class DoubleFieldProvider extends TypeFieldProvider[Double] {

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Double, baseField: GeneratedField[_, _]): InnerField[Double] =
    new BasicTypedField[Double, FD](baseField, op, _.toString, _.toDouble, "double")
}

class ByteFieldProvider extends TypeFieldProvider[Byte] {

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Byte, baseField: GeneratedField[_, _]): InnerField[Byte] =
    new BasicTypedField[Byte, FD](baseField, op, a => (a & 0xFF).toString, _.toByte, "byte")
}

class BooleanFieldProvider extends TypeFieldProvider[Boolean] {
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
}

class PasswordFieldProvider extends TypeFieldProvider[Password] {

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Password, baseField: GeneratedField[_, _]): InnerField[Password] =
    new BasicTypedField[Password, FD](baseField, op, _.get, Password, tpe = Some("password"))
}

class DateFieldProvider extends TypeFieldProvider[Date] {

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Date, baseField: GeneratedField[_, _]): InnerField[Date] = {

    val format = new SimpleDateFormat("dd/MM/yyyy")

    new BasicTypedField[Date, FD](baseField, op, format.format, format.parse, "date", Some("date"))
  }
}

class LongTextFieldProvider extends TypeFieldProvider[LongText] {
  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: LongText, baseField: FieldDescriptor#BaseFieldType[_, _]): InnerField[LongText] =
    new BasicTypedField[LongText, FD](baseField, op, _.value, LongText) {
      override protected val field: Elem = SHtml.ajaxTextarea(curValue, { s =>
        curValue = s
        baseField.toJsCmd(baseField.validate)
      }, "id" -> uniqueId)
    }
}