package com.soulever.makro.test

import com.soulever.makro.{BaseField, MFieldDescriptor}
import com.soulever.makro.types.Mapping
import com.soulever.makro

class FieldDescriptor extends MFieldDescriptor[TestForm] {
  override def enumFieldProvider[A <: Enumeration](enum: A): TypeFieldProvider[A#Value] =
    new TypeFieldProvider[A#Value] {

      override def empty: A#Value = enum.values.head

      override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD, i18nKey:String)(op: Option[A#Value]): FieldType[A#Value] =
        new TestField[A#Value] {
          override var value: A#Value = op.getOrElse(empty)
        }
    }

  override def mappingFieldProvider[A](mapping: List[(String, A)]): TypeFieldProvider[Mapping[A]] =
    new TypeFieldProvider[Mapping[A]] {
      override def empty: Mapping[A] = mapping.head._2

      override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD, i18nKey:String)(op: Option[Mapping[A]]): FieldType[Mapping[A]] =
        new TestField[Mapping[A]]() {
          override var value: Mapping[A] = op.getOrElse(empty)
        }
    }

  override def form(fields: List[FieldType[_]], buttons: List[ButtonType]): TestForm = TestForm()

  override def submitButton(label: String, clickAction: () => Unit): ButtonType = TestButton()

  override def field[A: Manifest, Obj](init: A,
                                       caption: String,
                                       innerField: (Option[A]) => FieldType[A],
                                       validators: List[(A) => Either[String, A]],
                                       secondaryValidators: List[(A, Obj) => Either[String, A]],
                                       css: String): BaseFieldType[A, Obj] =
    TestBaseField[A, Obj](init, caption, innerField, validators, secondaryValidators, css, innerValidations)

  def innerValidations:List[String] = List.empty

  override type BaseFieldType[A, Obj] = TestBaseField[A, Obj]
  override type ButtonType = TestButton
  override type FieldType[A] = TestField[A]
}

trait TypeFieldProvider[A] extends makro.TypeFieldProvider[A, TestField]

trait KindFieldProvider[A[_]] extends makro.KindFieldProvider[A, TestField]

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
                                 innerValidations:List[String]) extends BaseField[A, Obj]() {

  val inf = innerField(Option(init))

  override def getValue: A = inf.getValue

  override def isValid(obj: Obj): Boolean = {
    val right: Either[String, (A, Obj)] = Right[String, (A, Obj)](getValue -> obj)
    (secondaryValidators foldLeft right) {
      case (e, f) => e.right.flatMap(f.tupled).right.map(_ -> obj)
    }.isRight
  }

  override def isValid: Boolean =
    inf.
      validate.
      right.flatMap {
      a =>
        val right: Either[String, A] = Right[String, A](a)
        (validators foldLeft right) {
          case (e, f) => e.right.flatMap(f)
        }
    }.isRight
}

case class TestButton()