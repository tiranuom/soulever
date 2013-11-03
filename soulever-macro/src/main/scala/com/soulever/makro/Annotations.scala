package com.soulever.makro

import scala.annotation.StaticAnnotation

case class field(value:String = "") extends StaticAnnotation

trait FieldValidation[A] extends StaticAnnotation {
  def validate(a:A):Boolean
  def message:String
}

case class min[A : Ordering](value:A) extends FieldValidation[A]{
  def validate(a: A): Boolean = implicitly[Ordering[A]].lteq(value, a)

  def message: String = "min"
}

case class max[A : Ordering](value:A) extends FieldValidation[A]{
  def validate(a: A): Boolean = implicitly[Ordering[A]].lteq(a, value)

  def message: String = "max"
}

case class regex(value:String) extends FieldValidation[String]{
  def validate(a: String): Boolean = a.matches(value)

  def message: String = "regex"
}

case class nonEmpty() extends FieldValidation[String]{
  def validate(a: String): Boolean = !a.trim.isEmpty

  def message: String = "non-empty"
}

case class custom[A](value:A => Boolean, message:String) extends FieldValidation[A]{
  def validate(a: A): Boolean = value(a)
}

case class mapping[T, A](value:T => List[(String, A)]) extends StaticAnnotation