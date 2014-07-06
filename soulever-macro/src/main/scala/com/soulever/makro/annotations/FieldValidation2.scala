package com.soulever.makro
package annotations

import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox.Context

/**
 * @Auther tiran 
 * @Date 7/6/14.
 */
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
    if (!valid_?) c.error(field.pos,
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
