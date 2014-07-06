package com.soulever.lift

import java.io.File

import com.soulever.lift.providers.{EnumerationFieldProvider, MappingFieldProvider}
import com.soulever.lift.types.{GeneratedField, InnerField, TypeFieldProvider}
import com.soulever.makro.MFieldDescriptor
import com.soulever.makro.i18n.I18nKeyCollector
import com.soulever.makro.types.Mapping
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Props

import scala.io.Source
import scala.xml.{Elem, NodeSeq}

/**
 * @Auther tiran 
 * @Date 6/28/14.
 */
trait FieldDescriptor extends MFieldDescriptor[FieldDescriptor]{

  override type LayoutType = Elem

  override type ButtonType = NodeSeq

  override type FieldType[A] = InnerField[A]

  override type BaseFieldType[A, Obj] = GeneratedField[A, Obj]

  override def field[A: Manifest, Obj](init: A,
                                       caption: String,
                                       innerField: (A, BaseFieldType[A, Obj]) => FieldType[A],
                                       validators: List[(A) => Either[String, A]],
                                       secondaryValidators: List[(A, Obj) => Either[String, A]],
                                       css: String): BaseFieldType[A, Obj] =
    new GeneratedField[A, Obj](init, caption, innerField, validators, secondaryValidators, css, this)

  override def button(label: String, clickAction: () => Unit, fieldsList:List[GeneratedField[_, _]] = List.empty): ButtonType =
    SHtml.ajaxButton(i18n(label), () => {
      clickAction()
      fieldsList.map(_.collectUpdate).foldRight(JsCmd.unitToJsCmd())(_ & _)
    })

  override def form(fields: List[GeneratedField[_, _]], buttons: List[ButtonType]): Elem =
    <span>
      <table>
        <tbody>
          {fields.map(_.elem)}
          <tr><td></td><td>{buttons}</td><td></td></tr>
        </tbody>
      </table>
    </span>

  override def mappingFieldProvider[A](mapping: List[(String, A)]): TypeFieldProvider[Mapping[A], FieldDescriptor] = new MappingFieldProvider(mapping)

  override def enumFieldProvider[A <: Enumeration](enum: A): TypeFieldProvider[A#Value, FieldDescriptor] = new EnumerationFieldProvider(enum)

  override val i18nKeyCollector: I18nKeyCollector = new I18nKeyCollector(Props.get("i18n.print.path"))
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

  implicit val longTextFieldProvider = new LongTextFieldProvider

  implicit val optionKindFieldProvider = new OptionKindFieldProvider

  implicit val listKindFieldProvider = new ListKindFieldProvider
}