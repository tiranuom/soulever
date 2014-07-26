package com.soulever.vaadin

import java.util

import com.soulever.makro.AbstractFieldDescriptor
import com.soulever.makro.i18n.I18nKeyCollector
import com.soulever.makro.Soulever._
import com.soulever.vaadin.providers._
import com.soulever.vaadin.types.{GeneratedField, FieldProvider}
import com.typesafe.config.ConfigFactory
import com.vaadin.ui.Button.{ClickEvent, ClickListener}
import com.vaadin.ui._
import scala.language.experimental.macros

import scala.util.Try

trait FieldDescriptor extends AbstractFieldDescriptor[FieldDescriptor] {

  override type LayoutType = FormLayout

  override type ButtonType = Button

  override type FieldType[A] = AbstractField[A]

  override type BaseFieldType[A, Obj] = GeneratedField[A, Obj]

  override type RequestType = Unit

  def fieldComponent[A: Manifest, Obj](init: A,
                              caption: String,
                              innerField: (A, GeneratedField[A, Obj]) => AbstractField[A],
                              validators: List[(A) => Either[String, A]],
                              secondaryValidators:List[(A, Obj) => Either[String, A]],
                              css:String): GeneratedField[A, Obj] =
    new GeneratedField[A, Obj](init, caption, innerField, validators, secondaryValidators, css, this)

  override def formComponent(fields: List[GeneratedField[_, _]], buttons: List[Button]): FormLayout =
    new FormLayout(fields ::: List(new HorizontalLayout(buttons: _*)): _*)

  override def button(label:String, clickAction: () => Any, fieldsList:List[GeneratedField[_, _]] = List.empty): Button =
    new Button(i18n(label), new ClickListener {
      def buttonClick(event: ClickEvent) = clickAction()
    })

  def mappingFieldProvider[A](mapping: List[(String, A)]): FieldProvider[Mapping[A]] = new MappingFieldProvider[A](mapping)

  def enumFieldProvider[A <: Enumeration](enum: A): FieldProvider[A#Value] = new EnumerationFieldProvider[A](enum)

  override val i18nKeyCollector: I18nKeyCollector = new I18nKeyCollector(Props.printableFile)
}

object Props {

  val props = ConfigFactory.load(getClass.getClassLoader, "soulever.properties")

  def printableFile = Try(props.getString("i18n.print.path")).toOption.filter(_ =>isPrintable)

  def isPrintable = Try(props.getBoolean("i18n.print.on")).getOrElse(false)
}

trait FieldDescriptorImplicits extends com.soulever.makro.providers.FieldDescriptorImplicits {

  import com.soulever.makro.providers.FieldProviderMacros

  type FieldProvider[A] = com.soulever.vaadin.types.FieldProvider[A]

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

  implicit def enumFieldProvider[A <: Enumeration#Value]:FieldProvider[A] = macro FieldProviderMacros.enumFieldProviderMacro[A]
}

trait Descriptor extends FieldDescriptor with FieldDescriptorImplicits {

  override type FieldDescriptor = com.soulever.vaadin.FieldDescriptor

}