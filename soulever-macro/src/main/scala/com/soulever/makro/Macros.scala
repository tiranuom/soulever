package com.soulever.makro

import language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.{universe => ru}
import ru._

object Macros {
  def form[A <: Product, FD <: MFieldDescriptor[Rt], Rt](init:A, action:A => Either[Exception, A])
                                                       (implicit moduleDesc:FD):Rt = macro form_impl[A, FD, Rt]

  def form_impl[A:c.WeakTypeTag, FD:c.WeakTypeTag, Rt](c:Context)
                                                      (init:c.Expr[A], action:c.Expr[A => Either[Exception, A]])
                                                      (moduleDesc:c.Expr[FD]):c.Expr[Rt] = {

    import c.universe._

    val helper = new MacroHelper[c.type, FD, A](c)
    helper.fdWtt = Option(implicitly[WeakTypeTag[FD]])
    helper.initWtt = Option(implicitly[WeakTypeTag[A]])

    val beanTpe = implicitly[WeakTypeTag[A]].tpe

    val fields = beanTpe.typeSymbol.companion.typeSignature.members.collectFirst {
      case method if method.name.toString == "apply" => method
    }.toList.flatMap(_.asMethod.paramLists.flatten)

    val formFields = for {
      field <- fields if field.annotations.map(_.tree.tpe).contains(weakTypeOf[field])
      annotation <- field.annotations if annotation.tree.tpe =:= weakTypeOf[field]
      i18nKey <- annotation.tree.children.tail.toList
    } yield field

    val fieldExpansionData = formFields.map(helper.fieldExpansion(init))
    
    val fieldExpressions = fieldExpansionData.flatMap(_._3)

    val fieldListExpression = {
      val names = fieldExpansionData.map(a => q"${a._1}")
      q"List(..$names)"
    }

    val copyParams = {
      val fieldDataMap = fieldExpansionData.map{
        case (name, field, _) => field -> q"$name.getValue"
      }.toMap.withDefault(s => q"$init.${s.name.toTermName}")
      fields.map(fieldDataMap)
    }

    val submitButtonName = TermName(c.freshName())

    val comp = q""" val m = $moduleDesc
      import m._
      ..$fieldExpressions
      val fields = $fieldListExpression
      val $submitButtonName = submitButton(${c.literal(helper.toDotNotation(beanTpe.typeSymbol.name.toString) + ".submit")}, {() =>
        Option((fields.asInstanceOf[List[com.soulever.makro.BaseField[_, ${helper.initWtt.get}]]] foldLeft true){ case (b, f) => f.isValid && b }).
          filter(identity).
          flatMap{ _ =>
            Option($init.copy(..$copyParams)).
              filter(ob => (fields.asInstanceOf[List[com.soulever.makro.BaseField[_, ${helper.initWtt.get}]]] foldLeft true){ case (b, f) => f.isValid(ob) && b })
          }.foreach($action)
      })
      form(fields, List($submitButtonName))
    """

//    println( s"""comp = ${comp} """)

    c.Expr[Rt](comp)
  }
}

/**
 * //val valid = (fields.asInstanceOf[List[com.soulever.makro.BaseField[_, ${helper.initWtt.get}]]] foldLeft true){ case (b, f) => f.isValid && b } //casting is bad;fix this
        //if (valid) $action($init.copy(..$copyParams))
 */
