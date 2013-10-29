package com.soulever.makro

import scala.reflect.macros.Context
import language.experimental.macros

class MacroHelper[C <: Context](val c:C) {

  import c.universe._

  private def expandParameters(s: Type, collector: List[AppliedTypeTree] = List.empty)(implicit fieldImplType:Type): List[AppliedTypeTree] = {
    val TypeRef(_, _, args) = s
    args match {
      case Nil => tq"com.soulever.makro.TypeFieldProvider[$s, $fieldImplType]" :: collector
      case x :: Nil => expandParameters(x, tq"com.soulever.makro.KindFieldProvider[${s.typeConstructor}, $fieldImplType]" :: collector)
      case _ => tq"com.soulever.makro.TypeFieldProvider[$s, $fieldImplType]" :: collector
    }
  }

  def toDotNotation(s:String) = (s.tail foldLeft (s.head | 32).toChar.toString){(s, c) =>
    s + (if(c.isLower) c.toString else s".$c".toLowerCase)
  }

  def fieldExpansion[A : WeakTypeTag](init:Expr[A], beanName:Name, fieldImplType:Type)(field:Symbol) = {
    val innerField = {
      val types = expandParameters(field.typeSignature)(fieldImplType)
      (types.tail foldLeft q"implicitly[${types.head}].field(m)")((quo, tpe) => q"implicitly[$tpe].field($quo)(m)")
    }

    val fieldName = newTermName(c.fresh() + "Field")

    val i18nKey = c.literal(toDotNotation(beanName.toString) + "." + toDotNotation(field.name.toString))

    val validations = {
      val validations = field.annotations.filter(_.tpe <:< weakTypeOf[FieldValidation[_]])
      validations.foreach { v =>
        val fv = weakTypeOf[FieldValidation[_]].typeSymbol.asClass
        val inner = fv.typeParams(0).asType.toType.asSeenFrom(v.tpe, fv)
        val valid_? : Boolean = inner <:< field.typeSignature
        if (!valid_?) c.error(implicitly[WeakTypeTag[A]].tpe.typeSymbol.pos,
          s""" annotated validation ${v.tpe} in field ${implicitly[WeakTypeTag[A]].tpe.typeSymbol.fullName}.${field.name} is incompatible;
              | found    : FieldValidation[${field.typeSignature}]
              | required : FieldValidation[$inner]
              | """.stripMargin)
      }
      validations map {
        a => q""" { (x:${field.typeSignature}) =>
        val validator = ${a.tpe.typeSymbol.companionSymbol}(..${a.scalaArgs})
        Option(x).filter(validator.validate).toRight($i18nKey + s"[$${validator.message}]")
      }"""
      }
    }

    (fieldName, field, List(
      q"val $fieldName = m.field[${field.typeSignature}](${q"$init.${field.name}"}, $i18nKey, $innerField, List(..$validations), i18n = i18n)"))
  }


}
