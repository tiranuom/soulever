package com.soulever.makro

import scala.reflect.macros.{Universe, Context}
import language.experimental.macros

object Macros {
  def form[A <: Product, FD <: MFieldDescriptor[Rt], Rt](init:A, action:A => Either[Exception, A])
                                                       (implicit moduleDesc:FD):Rt = macro form_impl[A, FD, Rt]

  def form_impl[A:c.WeakTypeTag, FD:c.WeakTypeTag, Rt](c:Context)
                                                      (init:c.Expr[A], action:c.Expr[A => Either[Exception, A]])
                                                      (moduleDesc:c.Expr[FD]):c.Expr[Rt] = {

    val helper = new MacroHelper[c.type](c)

    import c.universe._

    val beanName = implicitly[WeakTypeTag[A]].tpe.typeSymbol.name

    val fields = implicitly[WeakTypeTag[A]].tpe.typeSymbol.companionSymbol.typeSignature.members.collectFirst {
      case method if method.name.toString == "apply" => method
    }.toList.flatMap(_.asMethod.paramss.flatten)

    val formFields = for {
      field <- fields if field.annotations.map(_.tpe).contains(weakTypeOf[field])
      annotation <- field.annotations if annotation.tpe =:= weakTypeOf[field]
      i18nKey <- annotation.scalaArgs.toList
    } yield field

    val fieldImplType = implicitly[WeakTypeTag[FD]].tpe.member(newTypeName("FieldType")).asType.toType.normalize.typeConstructor

    val fieldExpansionData = formFields.map(helper.fieldExpansion(init, beanName, fieldImplType))
    
    val fieldExpressions = fieldExpansionData.flatMap(_._3)

    val fieldListExpression = {
      val names = fieldExpansionData.map(a => q"${a._1}")
      q"List(..$names)"
    }

    val copyParams = {
      val fieldDataMap = fieldExpansionData.map{
        case (name, field, _) => field -> q"$name.getValue"
      }.toMap.withDefault(s => q"$init.${s.name}")
      fields.map(fieldDataMap)
    }

    val submitButtonName = newTermName(c.fresh())

    val comp = q""" val m = $moduleDesc
      import m._
      ..$fieldExpressions
      val fields = $fieldListExpression
      val $submitButtonName = submitButton(${c.literal(helper.toDotNotation(beanName.toString) + ".submit")}, {() =>
        val valid = (fields.asInstanceOf[List[com.soulever.makro.BaseField[_]]] foldLeft true){ case (b, f) => f.isValid && b } //casting is bad;fix this
        if (valid) $action($init.copy(..$copyParams))
      })
      form(fields, List($submitButtonName))
    """

    println( s"""comp = ${comp} """)

    c.Expr[Rt](comp)
  }
}

