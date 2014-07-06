package com.soulever.vaadin

import com.soulever.makro.i18n.I18nKeyCollector
import com.soulever.vaadin.types.{TypeFieldProvider, GeneratedField}
import com.typesafe.config.ConfigFactory
import com.vaadin.ui._
import com.vaadin.ui.Button.{ClickEvent, ClickListener}
import com.soulever.makro.MFieldDescriptor
import com.soulever.vaadin.providers._
import com.soulever.makro
import com.soulever.makro.types.Mapping

import scala.util.Try

trait FieldDescriptor extends MFieldDescriptor[FieldDescriptor] {

  type LayoutType = FormLayout

  type ButtonType = Button

  type FieldType[A] = AbstractField[A]

  type BaseFieldType[A, Obj] = GeneratedField[A, Obj]

  def field[A: Manifest, Obj](init: A,
                              caption: String,
                              innerField: (A, GeneratedField[A, Obj]) => AbstractField[A],
                              validators: List[(A) => Either[String, A]],
                              secondaryValidators:List[(A, Obj) => Either[String, A]],
                              css:String): FieldDescriptor#BaseFieldType[A, Obj] =
    new GeneratedField[A, Obj](init, caption, innerField, validators, secondaryValidators, css, this)

  override def form(fields: List[GeneratedField[_, _]], buttons: List[Button]): FormLayout =
    new FormLayout(fields ::: List(new HorizontalLayout(buttons: _*)): _*)

  override def button(label:String, clickAction: () => Unit, fieldsList:List[GeneratedField[_, _]] = List.empty): Button =
    new Button(i18n(label), new ClickListener {
      def buttonClick(event: ClickEvent) = clickAction()
    })

  def mappingFieldProvider[A](mapping: List[(String, A)]): TypeFieldProvider[Mapping[A], FieldDescriptor] = new MappingFieldProvider[A](mapping)

  def enumFieldProvider[A <: Enumeration](enum: A): TypeFieldProvider[A#Value, FieldDescriptor] = new EnumerationFieldProvider[A](enum)

  override val i18nKeyCollector: I18nKeyCollector = new I18nKeyCollector(Props.printableFile)
}

object Props {

  val props = ConfigFactory.load(getClass.getClassLoader, "soulever.properties")

  def printableFile = Try(props.getString("i18n.print.path")).toOption.filter(_ =>isPrintable)

  def isPrintable = Try(props.getBoolean("i18n.print.on")).getOrElse(false)
}

trait FieldDescriptorImplicits {

  implicit val stringFieldProvider = new TypeFieldProvider[String, FieldDescriptor] {
    override def empty: String = ""

    override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)(op: String, baseField: GeneratedField[_, _]): AbstractField[String] = {
      val field: TextField = new TextField()
      field.setValue(op)
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