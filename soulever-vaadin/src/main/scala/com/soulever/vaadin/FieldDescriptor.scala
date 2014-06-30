package com.soulever.vaadin

import com.vaadin.ui._
import com.vaadin.ui.Button.{ClickEvent, ClickListener}
import com.soulever.makro.MFieldDescriptor
import com.soulever.vaadin.providers._
import com.soulever.makro
import com.soulever.makro.types.Mapping

trait FieldDescriptor extends MFieldDescriptor[FieldDescriptor] {

  type LayoutType = FormLayout

  type ButtonType = Button

  type FieldType[A] = AbstractField[A]

  type BaseFieldType[A, Obj] = GeneratedField[A, Obj]

  def field[A: Manifest, Obj](init: A,
                              caption: String,
                              innerField: (Option[A], GeneratedField[A, Obj]) => AbstractField[A],
                              validators: List[(A) => Either[String, A]],
                              secondaryValidators:List[(A, Obj) => Either[String, A]],
                              css:String): FieldDescriptor#BaseFieldType[A, Obj] =
    new GeneratedField[A, Obj](init, caption, innerField, validators, secondaryValidators, css, i18n)

  def form(fields: List[AbstractField[_]], buttons: List[Button]): FormLayout =
    new FormLayout(fields ::: List(new HorizontalLayout(buttons: _*)): _*)

  def button(label:String, clickAction: () => Unit, fieldsList:List[AbstractField[_]] = List.empty): Button =
    new Button(i18n(label), new ClickListener {
      def buttonClick(event: ClickEvent) = clickAction()
    })

  def mappingFieldProvider[A](mapping: List[(String, A)]): makro.TypeFieldProvider[Mapping[A], AbstractField, FieldDescriptor] = new MappingFieldProvider[A](mapping)

  def enumFieldProvider[A <: Enumeration](enum: A): makro.TypeFieldProvider[A#Value, AbstractField, FieldDescriptor] = new EnumerationFieldProvider[A](enum)
}

trait FieldDescriptorImplicits {

  implicit val stringFieldProvider = new TypeFieldProvider[String, FieldDescriptor] {
    override def empty: String = ""

    override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)(op: Option[String], baseField: GeneratedField[_, _]): AbstractField[String] = {
      val field: TextField = new TextField()
      op.foreach(field.setValue)
      field
    }
  }

  implicit val intFieldProvider = new IntFieldProvider

  implicit val longFieldProvider = new LongFieldProvider

  implicit val floatFieldProvider = new FloatFieldProvider

  implicit val doubleFieldProvider = new DoubleFieldProvider

  implicit val booleanFieldProvider = new BooleanFieldProvider

  implicit val passwordFieldProvider = new PasswordFieldProvider

  implicit val byteFieldProvider = new ByteFieldProvider

  implicit val dateFieldProvider = new DateFieldProvider

  implicit val longTextFieldProvider = new LongTextFieldProvider

  implicit val optionFieldProvider = new OptionKindFieldProvider

  implicit val listFieldProvider = new ListKindFieldProvider
}