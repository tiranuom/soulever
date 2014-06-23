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

case class custom[A](value:A => Boolean, msg:String) extends FieldValidation[A]{
  def validate(a: A): Boolean = value(a)

  def message = msg
}

trait FieldValidation2[A, Obj] {
  def validate(a:A, obj:Obj):Boolean

  def message: String
}

case class fieldDependent[A, Obj](value: (A, Obj) => Boolean, message:String) extends FieldValidation2[A, Obj]{
  def validate(a:A, obj:Obj) = value(a, obj)
}

case class mapping[T, FD, A](value:(T, FD) => List[(String, A)]) extends StaticAnnotation

case class css(cls:String)