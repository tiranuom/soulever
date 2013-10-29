package com.soulever.makro

import scala.reflect.macros.{Universe, Context}
import language.experimental.macros

object Macros {
  def form[A <: Product, FD <: MFieldDescriptor[Rt], Rt](init:A, action:A => Either[Exception, A])
                                                       (implicit moduleDesc:FD):Rt = macro form_impl[A, FD, Rt]

  def form_impl[A:c.WeakTypeTag, FD:c.WeakTypeTag, Rt](c:Context)
                                                      (init:c.Expr[A], action:c.Expr[A => Either[Exception, A]])
                                                      (moduleDesc:c.Expr[FD]):c.Expr[Rt] = {
    import c.universe._

    def toDotNotation(s:String) = (s.tail foldLeft (s.head | 32).toChar.toString){(s, c) =>
      s + (if(c.isLower) c.toString else s".$c".toLowerCase)
    }

    val aName = implicitly[WeakTypeTag[A]].tpe.typeSymbol.name

    val fields = implicitly[WeakTypeTag[A]].tpe.typeSymbol.companionSymbol.typeSignature.members.collectFirst {
      case method if method.name.toString == "apply" => method
    }.toList.flatMap(_.asMethod.paramss.flatten)

    val formFields = for {
      field <- fields if field.annotations.map(_.tpe).contains(weakTypeOf[field])
      annotation <- field.annotations if annotation.tpe =:= weakTypeOf[field]
      i18nKey <- annotation.scalaArgs.toList
    } yield field

    val fieldImplType = implicitly[WeakTypeTag[FD]].tpe.member(newTypeName("FieldType")).asType.toType.normalize.typeConstructor

    val fieldDataExpansion = formFields.map {
      field =>
        def expandParameters(s: Type, collector: List[AppliedTypeTree] = List.empty): List[AppliedTypeTree] = {
          val TypeRef(_, _, args) = s
          args match {
            case Nil => tq"com.soulever.makro.TypeFieldProvider[$s, $fieldImplType]" :: collector
            case x :: Nil => expandParameters(x, tq"com.soulever.makro.KindFieldProvider[${s.typeConstructor}, $fieldImplType]" :: collector)
            case _ => tq"com.soulever.makro.TypeFieldProvider[$s, $fieldImplType]" :: collector
          }
        }

        val innerField = {
          val types = expandParameters(field.typeSignature)
          (types.tail foldLeft q"implicitly[${types.head}].field(m)")((quo, tpe) => q"implicitly[$tpe].field($quo)(m)")
        }

        val fieldName = newTermName(c.fresh() + "Field")

        val i18nKey = c.literal(toDotNotation(aName.toString) + "." + toDotNotation(field.name.toString))

        val validations = field.annotations.filter {
          a =>
            def tCheck = {
              val fv = weakTypeOf[FieldValidation[_]].typeSymbol.asClass
              val inner = fv.typeParams(0).asType.toType.asSeenFrom(a.tpe, fv)
              val valid_? : Boolean = inner <:< field.typeSignature
              if (!valid_?) c.error(implicitly[WeakTypeTag[A]].tpe.typeSymbol.pos, s""" annotated validation ${a.tpe} in field ${implicitly[WeakTypeTag[A]].tpe.typeSymbol.fullName}.${field.name} is incompatible;
              | found    : FieldValidation[${field.typeSignature}]
              | required : FieldValidation[$inner]
              | """.stripMargin)
              valid_?
            }
            a.tpe <:< weakTypeOf[FieldValidation[_]] && tCheck
        }.map{a =>
          q""" { (x:${field.typeSignature}) =>
        val validator = ${a.tpe.typeSymbol.companionSymbol}(..${a.scalaArgs})
        Option(x).filter(validator.validate).toRight($i18nKey + s"[$${validator.message}]")
      }"""
        }
        (fieldName, field, List(
          q"val $fieldName = m.field[${field.typeSignature}](${q"$init.${field.name}"}, $i18nKey, $innerField, List(..$validations), i18n = i18n)"))
    }

    val body = q"val m = $moduleDesc" ::
      q"import m._" ::
      fieldDataExpansion.flatMap(_._3)

    val fieldsList = {
      val names = fieldDataExpansion.map(a => q"${a._1}")
      q"List(..$names)"
    }

    val copyParams = {
      val fieldDataMap = fieldDataExpansion.map{
        case (name, field, _) => field -> q"$name.getValue"
      }.toMap.withDefault(s => q"$init.${s.name}")
      fields.map(fieldDataMap)
    }

    val submitButtonName = newTermName(c.fresh())

    val comp = q"""
      ..$body
      val fields = $fieldsList
      val $submitButtonName = submitButton(${c.literal(toDotNotation(aName.toString) + ".submit")}, {() =>
        val valid = (fields.asInstanceOf[List[com.soulever.makro.BaseField[_]]] foldLeft true){ case (b, f) => f.isValid && b } //casting is bad;fix this
        if (valid) $action($init.copy(..$copyParams))
      })
      form(fields, List($submitButtonName))
    """

    println( s"""comp = ${comp} """)

    c.Expr[Rt](comp)
  }
}

