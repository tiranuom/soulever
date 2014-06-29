package com.soulever.lift

import java.util.Date

import com.soulever.lift.types.{TypeFieldProvider, InnerField, GeneratedField}
import com.soulever.makro.types.{Password, Mapping}
import com.soulever.makro.MFieldDescriptor
import net.liftweb.http.js.JE.Str
import net.liftweb.http.js.JsCmds.SetElemById
import net.liftweb.http.{LiftRules, SHtml}
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.jquery.JqJE.{JqGetAttr, JqCss, JqAttr, JqId}

import scala.util.Try
import scala.xml.{NodeSeq, Elem}

/**
 * @Auther tiran 
 * @Date 6/28/14.
 */
trait FieldDescriptor extends MFieldDescriptor[FieldDescriptor]{

  override type LayoutType = NodeSeq

  override type ButtonType = NodeSeq

  override type FieldType[A] = InnerField[A]

  override type BaseFieldType[A, Obj] = GeneratedField[A, Obj]

  override def field[A: Manifest, Obj](init: A,
                                       caption: String,
                                       innerField: (Option[A], BaseFieldType[A, Obj]) => FieldType[A],
                                       validators: List[(A) => Either[String, A]],
                                       secondaryValidators: List[(A, Obj) => Either[String, A]],
                                       css: String): BaseFieldType[A, Obj] =
    new GeneratedField[A, Obj](init, caption, innerField, validators, secondaryValidators, css, i18n)

  override def button(label: String, clickAction: () => Unit): ButtonType =
    SHtml.ajaxButton(label, () => {
      clickAction()
      JsCmd.unitToJsCmd()
    })

  override def form(fields: List[InnerField[_]], buttons: List[ButtonType]): Elem =
    <span>
      <table>
        <tbody>
          {fields.map(_.elem)}
          <tr><td></td><td>{buttons}</td><td></td></tr>
        </tbody>
      </table>
    </span>

  override def mappingFieldProvider[A](mapping: List[(String, A)]): TypeFieldProvider[Mapping[A], FieldDescriptor] = ???

  override def enumFieldProvider[A <: Enumeration](enum: A): TypeFieldProvider[A#Value, FieldDescriptor] = ???
}

trait FieldDescriptorImplicits {
  implicit val stringFieldProvider = new TypeFieldProvider[String, FieldDescriptor] {

    override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                                 (op: Option[String], baseField:GeneratedField[_, _]): InnerField[String] =
      new InnerField[String] {

        var curValue = op.getOrElse(empty)
        private val text: Elem = SHtml.ajaxText(curValue, {s =>
          if(baseField.isValid) curValue = s
          baseField.toBeEvaluated
        })

        override def getValue: String = curValue

        override def setValue(value: String) = curValue = value

        override def elem: NodeSeq = text
      }

    override def empty: String = ""
  }

  implicit val intFieldProvider = new TypeFieldProvider[Int, FieldDescriptor] {

    override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                                 (op: Option[Int], baseField: GeneratedField[_, _]): InnerField[Int] =
      new BasicTypedField[Int, FD](baseField, op, empty, _.toString, _.toInt) {
        override def isValid: Boolean = {
          val t = Try(tempValue.toInt).isSuccess
          if(t) baseField.clearError else baseField.setError("integer")
          t
        }

        override def innerValidations: List[(String, String)] = List("integer" -> "integer")
      }

    override def empty: Int = 0
  }

  implicit val longFieldProvider = new TypeFieldProvider[Long, FieldDescriptor] {

    override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                                 (op: Option[Long], baseField: GeneratedField[_, _]): InnerField[Long] =
      new BasicTypedField[Long, FD](baseField, op, empty, _.toString, _.toLong) {
        override def isValid: Boolean = {
          val t = Try(tempValue.toInt).isSuccess
          if(t) baseField.clearError else baseField.setError("long")
          t
        }

        override def innerValidations: List[(String, String)] = List("long" -> "long")
      }

    override def empty: Long = 0
  }

  implicit val floatFieldProvider = new TypeFieldProvider[Float, FieldDescriptor] {

    override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                                 (op: Option[Float], baseField: GeneratedField[_, _]): InnerField[Float] =
      new BasicTypedField[Float, FD](baseField, op, empty, _.toString, _.toFloat) {
        override def isValid: Boolean = {
          val t = Try(tempValue.toInt).isSuccess
          if(t) baseField.clearError else baseField.setError("float")
          t
        }

        override def innerValidations: List[(String, String)] = List("float" -> "float")
      }

    override def empty: Float = 0
  }

  implicit val doubleFieldProvider = new TypeFieldProvider[Double, FieldDescriptor] {

    override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                                 (op: Option[Double], baseField: GeneratedField[_, _]): InnerField[Double] =
      new BasicTypedField[Double, FD](baseField, op, empty, _.toString, _.toDouble) {
        override def isValid: Boolean = {
          val t = Try(tempValue.toInt).isSuccess
          if(t) baseField.clearError else baseField.setError("double")
          t
        }

        override def innerValidations: List[(String, String)] = List("double" -> "double")
      }

    override def empty: Double = 0
  }

  implicit val booleanFieldProvider = new TypeFieldProvider[Boolean, FieldDescriptor] {
    override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                                 (op: Option[Boolean], baseField: GeneratedField[_, _]): InnerField[Boolean] = new InnerField[Boolean] {

      var state = op.getOrElse(empty)
      val uniqueId = LiftRules.funcNameGenerator()

      private val offLable: String = fieldDescriptor.i18n(baseField.i18nKey + "{off}")
      private val onLable: String = fieldDescriptor.i18n(baseField.i18nKey + "{on}")

      override def innerI18nKeys: List[(String, String)] = List("on" -> "On", "off" -> "Off")

      val field = SHtml.ajaxButton(if(state) onLable else offLable, () => {
        state = !state
        val (css, label) = if (state) "boolean-field-on" -> onLable else "boolean-field-off" -> offLable
        SetElemById(uniqueId, css, "className") &
          JqId(uniqueId).~>(JqAttr("value", Str(label))).cmd
      }, "id" -> uniqueId)

      override def getValue: Boolean = state

      override def setValue(value: Boolean): Unit = state = value

      override def elem: NodeSeq = field
    }

    override def empty: Boolean = true
  }

  implicit val passwordFieldProvider = new TypeFieldProvider[Password, FieldDescriptor] {

    override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                                 (op: Option[Password], baseField: GeneratedField[_, _]): InnerField[Password] = new InnerField[Password] {

      var curValue = op.getOrElse(empty).toString
      var tempValue = empty.toString
      private val text:Elem = SHtml.ajaxText(curValue, { s =>
        tempValue = s
        if (baseField.isValid) curValue = s
        baseField.toBeEvaluated
      }, "type" -> "password") // hack and security breaches. check proper way of doing this.

      def getValue: Password = curValue
      def setValue(value: Password): Unit = curValue = value
      def elem: NodeSeq = text
    }

    override def empty: Password = ""
  }

  implicit val byteFieldProvider = new TypeFieldProvider[Byte, FieldDescriptor] {

    override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                                 (op: Option[Byte], baseField: GeneratedField[_, _]): InnerField[Byte] =
      new BasicTypedField[Byte, FD](baseField, op, empty, a => (a & 0xFF).toString, _.toByte) {
        override def isValid: Boolean = {
          val t = Try(tempValue.toInt).isSuccess
          if(t) baseField.clearError else baseField.setError("byte")
          t
        }

        override def innerValidations: List[(String, String)] = List("byte" -> "byte")
      }

    override def empty: Byte = 0
  }

  implicit val dateFieldProvider = new TypeFieldProvider[Date, FieldDescriptor] {

    override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)(op: Option[Date], baseField: GeneratedField[_, _]): InnerField[Date] = new InnerField[Date] {

      override def getValue: Date = ???

      override def setValue(value: Date): Unit = ???

      override def elem: NodeSeq = ???
    }

    override def empty: Date = new Date()
  }

//  implicit val longTextFieldProvider = new LongTextFieldProvider

}

class BasicTypedField[A, FD <: MFieldDescriptor[_]](baseField: GeneratedField[_, _],
                                                    op:Option[A],
                                                    empty:A,
                                                    encode:A => String,
                                                    decode:String => A) extends InnerField[A]{
  var curValue = op.getOrElse(empty).toString
  var tempValue = empty.toString
  private val text:Elem = SHtml.ajaxText(curValue, { s =>
    tempValue = s
    if (baseField.isValid) curValue = s
    baseField.toBeEvaluated
  })
  def getValue: A = Try(decode(curValue)).getOrElse(empty)
  def setValue(value: A): Unit = curValue = value.toString
  def elem: NodeSeq = text
}