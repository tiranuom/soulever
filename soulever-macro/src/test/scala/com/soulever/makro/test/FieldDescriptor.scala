package com.soulever.makro.test

import com.soulever.makro.i18n.I18nKeyCollector
import com.soulever.makro.{BaseField, MFieldDescriptor}
import com.soulever.makro.types.Mapping
import com.soulever.makro

class FieldDescriptor extends MFieldDescriptor[FieldDescriptor] {
  override def enumFieldProvider[A <: Enumeration](enum: A): TypeFieldProvider[A#Value] =
    new TypeFieldProvider[A#Value] {

      override def empty: A#Value = enum.values.head

      override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                                   (op: A#Value, baseField: BaseFieldType[_, _]): TestField[A#Value] =
        new TestField[A#Value] {
          override var value: A#Value = op
        }
    }

  override def mappingFieldProvider[A](mapping: List[(String, A)]): TypeFieldProvider[Mapping[A]] =
    new TypeFieldProvider[Mapping[A]] {
      override def empty: Mapping[A] = mapping.head._2

      override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                                   (op: Mapping[A], baseField: BaseFieldType[_, _]): TestField[Mapping[A]] =
        new TestField[Mapping[A]]() {
          override var value: Mapping[A] = op
        }
    }



//  override def form(fields: List[FieldType[_]], buttons: List[ButtonType]): TestForm = TestForm()

//  override def button(label: String, clickAction: () => Unit): ButtonType = TestButton()

//  override def field[A: Manifest, Obj](init: A,
//                                       caption: String,
//                                       innerField: (Option[A]) => FieldType[A],
//                                       validators: List[(A) => Either[String, A]],
//                                       secondaryValidators: List[(A, Obj) => Either[String, A]],
//                                       css: String): BaseFieldType[A, Obj] =
//    TestBaseField[A, Obj](init, caption, innerField, validators, secondaryValidators, css, innerValidations)

  override def field[A: Manifest, Obj](init: A, caption: String, innerField: (A, BaseFieldType[A, Obj]) => FieldType[A], validators: List[(A) => Either[String, A]], secondaryValidators: List[(A, Obj) => Either[String, A]], css: String): BaseFieldType[A, Obj] = ???

  override def button(label: String, clickAction: () => Unit, fieldsList: List[BaseFieldType[_, _]]): ButtonType = ???

  override def form(fields: List[BaseFieldType[_, _]], buttons: List[ButtonType]): LayoutType = ???

  override val i18nKeyCollector: I18nKeyCollector = null

  def innerValidations:List[(String, String)] = List.empty

  override type BaseFieldType[A, Obj] = TestBaseField[A, Obj]
  override type ButtonType = TestButton
  override type FieldType[A] = TestField[A]
  override type LayoutType = TestForm
}

trait TypeFieldProvider[A] extends makro.providers.TypeFieldProvider[A, TestField, FieldDescriptor]

trait KindFieldProvider[A[_]] extends makro.providers.KindFieldProvider[A, TestField, FieldDescriptor]

trait FieldDescriptorImplicits {

}

case class TestForm()

trait TestField[T] {
  var value:T

  def getValue: T = value

  def validate:Either[String, T] = Right(value)
}

case class TestBaseField[A, Obj](init: A,
                                 caption: String,
                                 innerField: (Option[A]) => TestField[A],
                                 validators: List[(A) => Either[String, A]],
                                 secondaryValidators: List[(A, Obj) => Either[String, A]],
                                 css: String,
                                 innerValidations:List[(String, String)]) extends BaseField[A, Obj]() {

  val i18nKey = caption

  val inf = innerField(Option(init))

  override def value: A = inf.getValue

  override def valid_?(obj: Obj): Boolean = {
    val right: Either[String, (A, Obj)] = Right[String, (A, Obj)](value -> obj)
    (secondaryValidators foldLeft right) {
      case (e, f) => e.right.flatMap(f.tupled).right.map(_ -> obj)
    }.isRight
  }

  override def valid_?: Boolean =
    inf.
      validate.
      right.flatMap {
      a =>
        val right: Either[String, A] = Right[String, A](a)
        (validators foldLeft right) {
          case (e, f) => e.right.flatMap(f)
        }
    }.isRight

  override def innerI18nKeys: List[(String, String)] = ???

  override def value_=(value: A): Unit = ???
}

case class TestButton()