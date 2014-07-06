package com.soulever.makro

import com.soulever.metamacro.Meta

import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox.Context

case class field(value:String = "") extends StaticAnnotation

trait FieldBlockProvider {
  type AnnotationType
  def validate[A:c.WeakTypeTag](c:Context)(field:c.universe.Symbol)(v:c.universe.Annotation):Unit
  def generateCodeBlock[A:c.WeakTypeTag](c:Context)(field:c.universe.Symbol, i18nKey:c.universe.Tree)(v:c.universe.Annotation):(c.universe.Tree, c.universe.Tree)
}

trait ValidationMessageProvider {
  def message:String
  def defaultErrorMessage:String
}

trait FieldValidation[A] extends StaticAnnotation with ValidationMessageProvider {
  def validate(a:A):Boolean
}

object FieldValidation extends FieldBlockProvider {
  type AnnotationType = FieldValidation[_]

  def validate[A:c.WeakTypeTag](c:Context)(field:c.universe.Symbol)(v:c.universe.Annotation) {
    import c.universe._
    val fv = weakTypeOf[FieldValidation[_]].typeSymbol.asClass
    val initWtt = implicitly[WeakTypeTag[A]]
    val inner = fv.typeParams(0).asType.toType.asSeenFrom(v.tree.tpe, fv)
    val valid_? : Boolean = inner <:< field.typeSignature
    if (!valid_?) c.error(initWtt.tpe.typeSymbol.pos,
      s"""
              | annotated validation ${v.tree.tpe} in field ${initWtt.tpe.typeSymbol.fullName}.${field.name} is incompatible;
              | found    : FieldValidation[${field.typeSignature}]
              | required : FieldValidation[$inner]
              | """.stripMargin)
  }

  def generateCodeBlock[A:c.WeakTypeTag](c:Context)(field:c.universe.Symbol, i18nKey:c.universe.Tree)(a:c.universe.Annotation) = {
    import c.universe._
    val tr = if (a.tree.children.tail.length == 2) {
      q""" $i18nKey + "[" + ${a.tree.children.tail.last} + "]" """
    } else {
      q"""
            $i18nKey + "[" + ${a.tree.tpe.typeSymbol.companion}(..${a.tree.children.tail}).message + "]"
            """
    }
    val f = a.tree.children.tail match {
      case p1::p2::Nil => q"${a.tree.tpe.typeSymbol.companion}(null, $p2)"
      case p::Nil => q"${a.tree.tpe.typeSymbol.companion}($p)"
      case l => q"${a.tree.tpe.typeSymbol.companion}(..$l)"
    }//foreach(a => println(a + ":" + a.tpe.typeSymbol.name))
    ( f,
      q"""
          { (x:${field.typeSignature}) =>
            val validator = ${a.tree.tpe.typeSymbol.companion}(..${a.tree.children.tail})
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

case class nonEmpty() extends FieldValidation[String]{
  def validate(a: String): Boolean = !a.trim.isEmpty

  def message: String = "non-empty"

  override def defaultErrorMessage: String = "should not be empty"
}

case class custom[A](value:A => Boolean, message:String) extends FieldValidation[A]{
  def validate(a: A): Boolean = value(a)

  override def defaultErrorMessage: String = s"should not be []"
}

trait FieldValidation2[A, Obj] extends StaticAnnotation with ValidationMessageProvider{
  def validate(a:A, obj:Obj):Boolean
}

object FieldValidation2 extends FieldBlockProvider {
  override type AnnotationType = FieldValidation2[_, _]

  override def generateCodeBlock[A:c.WeakTypeTag](c: Context)(field: c.universe.Symbol, i18nKey: c.universe.Tree)(a: c.universe.Annotation): (c.universe.Tree, c.universe.Tree) = {
    import c.universe._
    val initWtt = implicitly[WeakTypeTag[A]]
    val tr = if (a.tree.children.tail.length == 2) {
      q""" $i18nKey + "[" + ${a.tree.children.tail.last} + "]" """
    } else {
      q"""
            $i18nKey + "[" + ${a.tree.tpe.typeSymbol.companion}(..${a.tree.children.tail}).message + "]"
            """
    }

    (q"${a.tree.tpe.typeSymbol.companion}(..${a.tree.children.tail})",
      q"""
        { (x:${field.typeSignature}, obj:${initWtt.tpe.finalResultType}) =>
          val validator = ${a.tree.tpe.typeSymbol.companion}(..${a.tree.children.tail})
          Option(x).filter(x => validator.validate(x, obj)).toRight($i18nKey + s"[$${validator.message}]")
        }""")
  }

  override def validate[A: c.WeakTypeTag](c: Context)(field: c.universe.Symbol)(v: c.universe.Annotation): Unit = {
    import c.universe._
    val initWtt = implicitly[WeakTypeTag[A]]
    val fv = weakTypeOf[FieldValidation2[_, _]].typeSymbol.asClass
    val inner = fv.typeParams(0).asType.toType.asSeenFrom(v.tree.tpe, fv)
    val valid_? : Boolean = inner <:< field.typeSignature
    if (!valid_?) c.error(initWtt.tpe.typeSymbol.pos,
      s""" annotated validation ${v.tree.tpe} in field ${initWtt.tpe.typeSymbol.fullName}.${field.name} is incompatible;
              | found    : FieldValidation[${field.typeSignature}, _]
              | required : FieldValidation[$inner, Init]
              | """.stripMargin)
  }
}

case class fieldDependent[A, Obj](value: (A, Obj) => Boolean, message:String) extends FieldValidation2[A, Obj]{
  def validate(a:A, obj:Obj) = value(a, obj)

  override def defaultErrorMessage: String = s"should be [$message]"
}

case class mapping[FD, A](value:(FD) => List[(String, A)]) extends StaticAnnotation

case class css(cls:String)