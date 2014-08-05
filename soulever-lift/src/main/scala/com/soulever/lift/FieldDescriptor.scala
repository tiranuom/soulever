package com.soulever.lift

import java.io.File

import com.soulever.lift.providers.{EnumerationFieldProvider, MappingFieldProvider}
import com.soulever.lift.types.{GeneratedFieldProvider, GeneratedField, InnerField, FieldProvider}
import com.soulever.makro.AbstractFieldDescriptor
import com.soulever.makro.Soulever._
import com.soulever.makro.i18n.I18nKeyCollector
import net.liftweb.http.js.JsCmd
import net.liftweb.http.{S, SHtml}
import net.liftweb.util.Props
import scala.language.experimental.macros

import scala.xml.{Elem, NodeSeq}

/**
 * @Auther tiran 
 * @Date 6/28/14.
 */
trait FieldDescriptor extends AbstractFieldDescriptor[FieldDescriptor]{

  override type LayoutType = Elem

  override type ButtonType = NodeSeq

  override type FieldType[A] = InnerField[A]

  override type BaseFieldType[A, Obj] = GeneratedField[A, Obj]

  override type RequestType = S.type

  override def fieldComponent[A: Manifest, Obj](init: A,
                                       caption: String,
                                       innerField: (A, BaseFieldType[A, Obj]) => FieldType[A],
                                       validators: List[(A) => Either[String, A]],
                                       secondaryValidators: List[(A, Obj) => Either[String, A]],
                                       css: String): BaseFieldType[A, Obj] =
    new GeneratedField[A, Obj](init, caption, innerField, validators, secondaryValidators, css, this)

  override def button(label: String, clickAction: () => Any, fieldsList:List[GeneratedField[_, _]] = List.empty): ButtonType =
    SHtml.ajaxButton(i18n(label), () => {
      clickAction()
      fieldsList.map(_.collectUpdate).foldRight(JsCmd.unitToJsCmd())(_ & _)
    })

  override def formComponent(fields: List[GeneratedField[_, _]], buttons: List[ButtonType]): Elem =
    <span>
      <table>
        <tbody>
          {fields.map(_.elem)}
          <tr><td></td><td>{buttons}</td><td></td></tr>
        </tbody>
      </table>
    </span>

  override def mappingFieldProvider[A](mapping: List[(String, A)]): GeneratedFieldProvider[Mapping[A]] = new MappingFieldProvider(mapping)

  override def enumFieldProvider[A <: Enumeration](enum: A): FieldProvider[A#Value] = new EnumerationFieldProvider(enum)

  override val i18nKeyCollector: I18nKeyCollector = new I18nKeyCollector(Props.get("i18n.print.path").map( new File(".").getAbsolutePath + _))
}

trait LowPriorityFieldDescriptorImplicits extends com.soulever.makro.providers.FieldDescriptorImplicits {

  type FieldProvider[A] = com.soulever.lift.types.FieldProvider[A]

  type GeneratedFieldProvider[A] = com.soulever.lift.types.GeneratedFieldProvider[A]

  override type FieldDescriptor = com.soulever.lift.FieldDescriptor

  implicit def generatedFieldProvider[A](implicit $ev:GeneratedFieldProvider[A]) = new FieldProvider[A] {
    override def field[FD <: FieldDescriptor](fieldDescriptor: FD)
                                             (op: A, baseField: FD#BaseFieldType[_, _]): InnerField[A] =
      implicitly[GeneratedFieldProvider[A]].field(fieldDescriptor)(op, baseField)
  }

  import com.soulever.makro.providers.FieldProviderMacros

  implicit def implicitEnumFieldProvider[A <: Enumeration#Value]:FieldProvider[A] = macro FieldProviderMacros.enumFieldProviderMacro[A]
}

trait FieldDescriptorImplicits extends LowPriorityFieldDescriptorImplicits {
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

  implicit def optionFieldProvider[A : FieldProvider : EmptyProvider] = new OptionFieldProvider[A]

  implicit def listFieldProvider[A : FieldProvider : EmptyProvider] = new ListFieldProvider[A]
}

trait Descriptor extends FieldDescriptor with FieldDescriptorImplicits