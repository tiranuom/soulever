package com.soulever.lift

import com.soulever.lift.types.{GeneratedField, InnerField, TypeFieldProvider}
import com.soulever.makro.MFieldDescriptor
import com.soulever.makro.types.Mapping
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd

import scala.xml.{Elem, NodeSeq}

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

  override def button(label: String, clickAction: () => Unit, fieldsList:List[InnerField[_]] = List.empty): ButtonType =
    SHtml.ajaxButton(label, () => {
      clickAction()
      fieldsList.map(_.updateJs).foldRight(JsCmd.unitToJsCmd())(_ & _)
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
  import com.soulever.lift.providers._

  implicit val stringFieldProvider = new StringFieldProvider

  implicit val intFieldProvider = new IntFieldProvider

  implicit val longFieldProvider = new LongFieldProvider

  implicit val floatFieldProvider = new FloatFieldProvider

  implicit val doubleFieldProvider = new DoubleFieldProvider

  implicit val booleanFieldProvider = new BooleanFieldProvider

  implicit val passwordFieldProvider = new PasswordFieldProvider

  implicit val byteFieldProvider = new ByteFieldProvider

  implicit val dateFieldProvider = new DateFieldProvider

  //  implicit val longTextFieldProvider = new LongTextFieldProvider

}