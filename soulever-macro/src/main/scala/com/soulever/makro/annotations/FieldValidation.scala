package com.soulever.makro
package annotations

import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

/**
 * @Auther tiran 
 * @Date 7/6/14.
 */

trait FieldValidation[A] extends StaticAnnotation with ValidationMessageProvider with (A =/> String) {
  def validate(a:A):Boolean

  override def isDefinedAt(x: A): Boolean = !validate(x)

  override def apply(v1: A): String = defaultErrorMessage
}

object FieldValidation extends FieldBlockProvider {

  def f(a:Int) = (min(0) orElse max(60) lift)(a).toLeft(a)

  type AnnotationType = FieldValidation[_]

  def validate[A:c.WeakTypeTag](c:Context)(field:c.universe.Symbol)(v:c.universe.Annotation) {
    import c.universe._
    val fv = weakTypeOf[FieldValidation[_]].typeSymbol.asClass
    val initWtt = implicitly[WeakTypeTag[A]]
    val inner = fv.typeParams(0).asType.toType.asSeenFrom(v.tree.tpe, fv)
    val valid_? : Boolean = inner <:< field.typeSignature
    if (!valid_?) c.error(field.pos,
      s"""
              | annotated validation `${v.tree.tpe}` in field `${initWtt.tpe.typeSymbol.fullName}.${field.name}` is incompatible;
              | found    : FieldValidation[${field.typeSignature}]
              | required : FieldValidation[$inner]
              | """.stripMargin)
  }

  def generateCodeBlock[A:c.WeakTypeTag](c:Context)(field:c.universe.Symbol, i18nKey:c.universe.Tree)(a:c.universe.Annotation) = {
    import c.universe._
    val f = a.tree.children.tail match {
      case p1::p2::Nil => q"${a.tree.tpe.typeSymbol.companion}[..${a.tree.tpe.typeArgs}](null, $p2)"
      case p::Nil => q"${a.tree.tpe.typeSymbol.companion}[..${a.tree.tpe.typeArgs}]($p)"
      case l => q"${a.tree.tpe.typeSymbol.companion}[..${a.tree.tpe.typeArgs}](..$l)"
    }//foreach(a => println(a + ":" + a.tpe.typeSymbol.name))
    (
      q"""{
          val validation = $f
          validation.message -> validation.defaultErrorMessage
         }
       """,
      q"""
          { (x:${field.typeSignature}) =>
            val validator = ${a.tree.tpe.typeSymbol.companion}[..${a.tree.tpe.typeArgs}](..${a.tree.children.tail})
            Option(x).filter(validator.validate).toRight($i18nKey + s"[$${validator.message}]")
          }""")
  }
}

case class min[A : Ordering](value:A) extends FieldValidation[A]{
  def validate(a: A): Boolean = implicitly[Ordering[A]].lteq(value, a)

  def message: String = "min"

  override def defaultErrorMessage: String = s"should be greater than $value"
}

case class max[A : Ordering](value:A) extends FieldValidation[A]{
  def validate(a: A): Boolean = implicitly[Ordering[A]].lteq(a, value)

  def message: String = "max"

  override def defaultErrorMessage: String = s"should be lesser than $value"
}

case class regex(value:String) extends FieldValidation[String]{
  def validate(a: String): Boolean = a.matches(value)

  def message: String = "regex"

  override def defaultErrorMessage: String = s"should match with $value"
}

trait EmptyCheckProvider[A] {
  def isEmpty(a:A):Boolean
}

object EmptyCheckProvider {
  implicit def materializeEmptyCheckProvider[A]:EmptyCheckProvider[A] = macro materializeEmptyCheckProvider_impl[A]

  def materializeEmptyCheckProvider_impl[A:c.WeakTypeTag](c:Context) = {
    import c.universe._
    val tag: WeakTypeTag[A] = implicitly[WeakTypeTag[A]]
    q"""
       new EmptyCheckProvider[${tag.tpe}] {
         def isEmpty(a:${tag.tpe}) = a.isEmpty
       }
     """
  }
}

case class nonEmpty[A : EmptyCheckProvider]() extends FieldValidation[A]{
  def validate(a: A): Boolean = !implicitly[EmptyCheckProvider[A]].isEmpty(a)//!a.trim.isEmpty

  def message: String = "non-empty"

  override def defaultErrorMessage: String = "should not be empty"
}

case class custom[A](value:A => Boolean, message:String) extends FieldValidation[A]{
  def validate(a: A): Boolean = value(a)

  override def defaultErrorMessage: String = s"should not be []"
}


