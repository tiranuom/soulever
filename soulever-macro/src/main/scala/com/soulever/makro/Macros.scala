package com.soulever.makro

import language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.{universe => ru}

object Macros {
  def form[ClassType <: Product, FD <: MFieldDescriptor[Rt], Rt](init:ClassType, action:ClassType => Either[Exception, ClassType])
                                                                (implicit moduleDesc:FD):Rt = macro MacrosImpl.form_impl[ClassType, FD, Rt]

  def field[FieldType, FD <: MFieldDescriptor[_], ClassType](init:FieldType,
                                                             i18nKey:String,
                                                             ths:ClassType)
                                                            (implicit moduleDesc:FD):FD#BaseFieldType[FieldType, ClassType] = macro MacrosImpl.field_impl[FieldType, FD, ClassType]
}

class MacrosImpl(val c:Context) {
  import c.universe._
  def form_impl[ClassType:c.WeakTypeTag, FD:c.WeakTypeTag, Rt](init:c.Expr[ClassType], action:c.Expr[ClassType => Either[Exception, ClassType]])
                                                              (moduleDesc:c.Expr[FD]) = {


    val initWtt = implicitly[WeakTypeTag[ClassType]]

    val fields = initWtt.tpe.typeSymbol.companion.typeSignature.members.collectFirst {
      case method if method.name.toString == "apply" => method
    }.toList.flatMap(_.asMethod.paramLists.flatten)

    val i18nPrefix = toDotNotation(initWtt.tpe.typeSymbol.name.toString.trim)

    val (fieldExpressions, fieldListExpression, copyParamCodes) = fields.collect{
      case field if field.annotations.map(_.tree.tpe).contains(weakTypeOf[field]) =>
        val (code, fieldName) = generateFieldBlock[FD, ClassType](q"$init.${field.name.toTermName}", field, i18nPrefix, q"${i18nPrefix + "." + toDotNotation(field.name.toString.trim)}", init)
        (code, q"$fieldName", field -> q"$fieldName.getValue")
    }.unzip3(identity)

    val copyParams = fields.map(copyParamCodes.toMap.withDefault(s => q"$init.${s.name.toTermName}"))

    val (buttonName, buttonCode) = {
      val buttonName = TermName(c.freshName())
      val submitButtonKey = s"$i18nPrefix.submit"

      buttonName -> q"""
          val $buttonName = {
            I18nKeyCollector.insert($i18nPrefix)($submitButtonKey)
            val buttonImpl = button($submitButtonKey, {() =>
              Option((fields.asInstanceOf[List[com.soulever.makro.BaseField[_, $initWtt]]] foldLeft true){ case (b, f) => f.isValid && b }).
              filter(identity).
              flatMap{ _ =>
                Option($init.copy(..$copyParams)).
                filter(ob => (fields.asInstanceOf[List[com.soulever.makro.BaseField[_, $initWtt]]] foldLeft true){ case (b, f) => f.isValid(ob) && b })
              }.foreach($action)
            })
            buttonImpl
          }
       """
    }

    val comp = q"""
    val m = $moduleDesc
      import m._
      ..${fieldExpressions.flatten}
      val fields = $fieldListExpression
      $buttonCode
      form(fields, List($buttonName))
    """
    comp
  }

  def toDotNotation(s:String) =
    s.replaceAll("(?<=[A-Z])(?=[A-Z][a-z])|(?<=[^A-Z])(?=[A-Z])|(?<=[A-Za-z])(?=[^A-Za-z])", ".").toLowerCase.trim

  def field_impl[FieldType:c.WeakTypeTag, FD:c.WeakTypeTag, ClassType](init:c.Expr[FieldType],
                                                                       i18nKey:c.Expr[String],
                                                                       ths:c.Expr[ClassType])
                                                                      (moduleDesc:c.Expr[FD]) = {

    val (code, fieldName) = generateFieldBlock[FD, ClassType](init.tree, init.tree.symbol, "", i18nKey.tree, ths)

    q"""{
        val m = $moduleDesc
        import m._
       ..$code
       $fieldName
       }
      """
  }

  def generateFieldBlock[FD:c.WeakTypeTag, ClassType: c.WeakTypeTag](init: Tree,
                                                                     fieldSymbol: Symbol,
                                                                     i18nPrefix: String,
                                                                     i18nKey: Tree,
                                                                     ths:c.Expr[ClassType]) = {


    val ftt = fieldSymbol.typeSignature

    val fieldImplType = implicitly[WeakTypeTag[FD]].tpe.member(TypeName("FieldType")).asType.toType.dealias.typeConstructor

    val fieldName = TermName(c.freshName() + "Field")

    val css = Option(fieldSymbol).flatMap(_.annotations.collectFirst {
      case s if s.tree.tpe.typeConstructor =:= c.weakTypeOf[com.soulever.makro.css] => s.tree.children.tail.head
    })

    val innerField = {

      val mapping = Option(fieldSymbol).flatMap(_.annotations.collectFirst {
        case s if s.tree.tpe.typeConstructor =:= weakTypeOf[com.soulever.makro.mapping[Any, Any]].typeConstructor =>
          q"${s.tree.children.tail.head}(m)"
      })

      def expandParameters(s: Type = ftt, collector: List[Tree] = List.empty): List[Tree] = {
        val TypeRef(pre, _, args) = s
        args match {
          case Nil if s <:< weakTypeOf[Enumeration#Value] =>
            q"enumFieldProvider[$pre](${pre.termSymbol})" :: collector
          case Nil =>
            q"implicitly[com.soulever.makro.TypeFieldProvider[$s, $fieldImplType]]" :: collector
          case x :: Nil if s.typeConstructor =:= c.weakTypeOf[com.soulever.makro.types.Mapping[Any]].typeConstructor =>
            if (mapping.isEmpty) c.error(init.pos, "Cannot find mapping for the given type")
            q"mappingFieldProvider[$x]($mapping.getOrElse(List.empty))" :: collector
          case x :: Nil =>
            expandParameters(x, q"implicitly[com.soulever.makro.KindFieldProvider[${s.typeConstructor}, $fieldImplType]]" :: collector)
          case _ => q"implicitly[com.soulever.makro.TypeFieldProvider[$s, $fieldImplType]]" :: collector
        }
      }

      val types = expandParameters()
      (types.tail foldLeft (q"${types.head}.field(m, $i18nKey)", q"${types.head}.empty")){
        case (quo, tpe) =>
          (q"$tpe.field(${quo._1}, ${quo._2})(m, $i18nKey)", q"$tpe.empty")
      }._1
    }

    val (validationMessages, validators) = {
      val provided = Option(fieldSymbol).toList.flatMap(_.annotations.filter(_.tree.tpe <:< weakTypeOf[FieldValidation.AnnotationType]))
      provided.foreach(FieldValidation.validate[ClassType](c)(fieldSymbol))
      provided.map(FieldValidation.generateCodeBlock[ClassType](c)(fieldSymbol, q"$i18nKey"))
    }.unzip(identity)

    val (classDependentValidationMessages, classDependentValidators) = {
      val provided = Option(fieldSymbol).toList.flatMap(_.annotations.filter(_.tree.tpe <:< weakTypeOf[FieldValidation2.AnnotationType]))
      provided.foreach(FieldValidation2.validate[ClassType](c)(fieldSymbol))
      provided.map(FieldValidation2.generateCodeBlock[ClassType](c)(fieldSymbol, q"$i18nKey"))
    }.unzip(identity)

    List(
      q"""
      val $fieldName = {
        I18nKeyCollector.insert($i18nPrefix)($i18nKey)
        ($validationMessages ::: $classDependentValidationMessages).foreach(I18nKeyCollector.insert($i18nPrefix))
        val field = m.field[$ftt, ${ths.actualType}]($init, $i18nKey.trim, $innerField, $validators, $classDependentValidators, $css.getOrElse(""))
        field.asInstanceOf[com.soulever.makro.BaseField[_,_]].innerValidations.map(s => $i18nKey + "[" + s._1 + "]").foreach(I18nKeyCollector.insert($i18nPrefix))
        field.asInstanceOf[com.soulever.makro.BaseField[_,_]].innerI18nKeys.map(s => $i18nKey + "{" + s._1 + "}").foreach(I18nKeyCollector.insert($i18nPrefix))
        field
      }
      """) -> fieldName
  }
}