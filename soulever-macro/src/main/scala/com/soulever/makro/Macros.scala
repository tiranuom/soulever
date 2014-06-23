package com.soulever.makro

import language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.{universe => ru}
import ru._

object Macros {
  def form[A <: Product, FD <: MFieldDescriptor[Rt], Rt](init:A, action:A => Either[Exception, A])
                                                        (implicit moduleDesc:FD):Rt = macro MacrosImpl.form_impl[A, FD, Rt]
}

class MacrosImpl(val c:Context) {
  import c.universe._
  def form_impl[A:c.WeakTypeTag, FD:c.WeakTypeTag, Rt](init:c.Expr[A], action:c.Expr[A => Either[Exception, A]])
                                                      (moduleDesc:c.Expr[FD]) = {


    val initWtt = implicitly[WeakTypeTag[A]]

    val beanTpe = initWtt.tpe

    val fields = beanTpe.typeSymbol.companion.typeSignature.members.collectFirst {
      case method if method.name.toString == "apply" => method
    }.toList.flatMap(_.asMethod.paramLists.flatten)

    val formFields = for {
      field <- fields if field.annotations.map(_.tree.tpe).contains(weakTypeOf[field])
    } yield field

    val fieldExpansionData = formFields.map(fieldExpansion[A, FD](init))

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

    val comp = q"""
    val m = $moduleDesc
      import m._
      ..$fieldExpressions
      val fields = $fieldListExpression
      val $submitButtonName = submitButton(${c.literal(toDotNotation(beanTpe.typeSymbol.name.toString) + ".submit")}, {() =>
        Option((fields.asInstanceOf[List[com.soulever.makro.BaseField[_, $initWtt]]] foldLeft true){ case (b, f) => f.isValid && b }).
          filter(identity).
          flatMap{ _ =>
            Option($init.copy(..$copyParams)).
              filter(ob => (fields.asInstanceOf[List[com.soulever.makro.BaseField[_, $initWtt]]] foldLeft true){ case (b, f) => f.isValid(ob) && b })
          }.foreach($action)
      })
      form(fields, List($submitButtonName))
    """

    println( s"""comp = ${comp} """)

    comp
  }

  def toDotNotation(s:String) = s.replaceAll(
    String.format("%s|%s|%s", "(?<=[A-Z])(?=[A-Z][a-z])", "(?<=[^A-Z])(?=[A-Z])", "(?<=[A-Za-z])(?=[^A-Za-z])"),
    "."
  ).toLowerCase.trim

  def fieldExpansion[A :c.WeakTypeTag, FD:c.WeakTypeTag](init:c.Expr[A])(field:Symbol) = {

    val fdWtt = implicitly[WeakTypeTag[FD]]
    val initWtt = implicitly[WeakTypeTag[A]]
    val fieldImplType = fdWtt.tpe.member(TypeName("FieldType")).asType.toType.dealias.typeConstructor

    val fieldName = TermName(c.freshName() + "Field")

    val css = field.annotations.collectFirst {
      case s if s.tree.tpe.typeConstructor == c.weakTypeOf[com.soulever.makro.css]  =>
        s.tree.children.tail.head
    }

    val i18nKey = q"${toDotNotation(initWtt.tpe.typeSymbol.name.toString.trim) + "." + toDotNotation(field.name.toString.trim)}"

    val innerField = {

      val mapping = field.annotations.collectFirst {
        case s if s.tree.tpe.typeConstructor =:= weakTypeOf[com.soulever.makro.mapping[Any, Any, Any]].typeConstructor =>
          val f = s.tree.children.tail.head
          q"$f($init, m)"
      }

      def expandParameters(s: Type, collector: List[Tree] = List.empty): List[Tree] = {
        val TypeRef(pre, _, args) = s
        args match {
          case Nil if s <:< weakTypeOf[Enumeration#Value] =>
            q"enumFieldProvider[$pre](${pre.termSymbol})" :: collector
          case Nil =>
            q"implicitly[com.soulever.makro.TypeFieldProvider[$s, $fieldImplType]]" :: collector
          case x :: Nil if s.typeConstructor =:= c.weakTypeOf[com.soulever.makro.types.Mapping[Any]].typeConstructor =>
            if (mapping.isEmpty) c.error(initWtt.tpe.typeSymbol.pos, "Cannot find mapping for the given type")
            q"mappingFieldProvider[$x](${mapping.get})" :: collector
          case x :: Nil =>
            expandParameters(x, q"implicitly[com.soulever.makro.KindFieldProvider[${s.typeConstructor}, $fieldImplType]]" :: collector)
          case _ => q"implicitly[com.soulever.makro.TypeFieldProvider[$s, $fieldImplType]]" :: collector
        }
      }

      val types = expandParameters(field.typeSignature)
      (types.tail foldLeft (q"${types.head}.field(m, $i18nKey)", q"${types.head}.empty")){
        case (quo, tpe) =>
          (q"$tpe.field(${quo._1}, ${quo._2})(m, $i18nKey)", q"$tpe.empty")
      }._1
    }

    def generateTree(provider:FieldBlockProvider) = {
      val provided = field.annotations.filter(_.tree.tpe <:< weakTypeOf[provider.AnnotationType])
      provided.foreach(provider.validate[A](c)(field))
      provided.map(provider.generateCodeBlock[A](c)(field, i18nKey))
    }

    val validations = generateTree(FieldValidation)

    val validations2 = generateTree(FieldValidation2)

    (fieldName, field, List(
      q"""
      val $fieldName = {
        val field = m.field[${field.typeSignature}, $initWtt](${q"$init.${field.name.toTermName}"}, $i18nKey.trim, $innerField, List(..${validations.map(_._2)}), List(..${validations2.map(_._2)}), ${css.getOrElse(q""" "" """)})
        List(..${validations.map(_._1)}, ..${validations2.map(_._1)}).filter(_ != null).foreach(I18nKeyCollector.insert)
        I18nKeyCollector.insert($i18nKey)
        field.asInstanceOf[com.soulever.makro.BaseField[_,_]].innerValidations.map(s => $i18nKey + "[" + s + "]").foreach(I18nKeyCollector.insert)
        field
      }
      """))
  }
}