package com.soulever.vaadin

import com.vaadin.ui._
import com.vaadin.ui.Button.{ClickEvent, ClickListener}
import com.soulever.makro.MFieldDescriptor
import com.soulever.vaadin.providers._

trait FieldDescriptor extends MFieldDescriptor[FormLayout] {
  type ButtonType = Button

  type FieldType[A] = AbstractField[A]

  type BaseFieldType[A] = GeneratedField[A]

  def field[A: Manifest](init: A,
                         caption: String,
                         innerField: FieldDescriptor#FieldType[A],
                         validators: List[(A) => Either[String, A]],
                         prefix: String,
                         postfix: String,
                         i18n:String => String = identity): FieldDescriptor#BaseFieldType[A] =
    new GeneratedField[A](init, caption, innerField, validators, prefix, postfix, i18n)

  def form(fields: List[FieldDescriptor#FieldType[_]], buttons: List[FieldDescriptor#ButtonType]): FormLayout =
    new FormLayout(fields ::: buttons :_*)

  def submitButton(label:String, clickAction: () => Unit): FieldDescriptor#ButtonType =
    new Button(i18n(label), new ClickListener {
      def buttonClick(event: ClickEvent) = clickAction()
    })
}

class FieldDescriptorImplicits {

  implicit val stringFieldProvider = new TypeFieldProvider[String] {
    def field[FD <: MFieldDescriptor[_]](implicit fieldDescriptor: FD): AbstractField[String] = new TextField()
  }

  implicit val intFieldProvider = new IntFieldProvider

  implicit val longFieldProvider = new LongFieldProvider

  implicit val floatFieldProvider = new FloatFieldProvider

  implicit val doubleFieldProvider = new DoubleFieldProvider

  implicit val booleanFieldProvider = new BooleanFieldProvider

  implicit val optionFieldProvider = new OptionKindFieldProvider
}