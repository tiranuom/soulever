package com.soulever.makro

import language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def form[ClassType <: Product, FD <: MFieldDescriptor[_]](caseClass:ClassType, action:ClassType => Either[Exception, ClassType])
                                                                (implicit moduleDesc:FD):FD#LayoutType= macro MacrosImpl.form_impl[ClassType, FD]

  def field[FieldType, FD <: MFieldDescriptor[_], ClassType](value:FieldType,
                                                             i18nKey:String,
                                                             ths:ClassType)
                                                            (implicit moduleDesc:FD):FD#BaseFieldType[FieldType, ClassType] = macro MacrosImpl.field_impl[FieldType, FD, ClassType]
}

class MacrosImpl(val c:Context) {
  import c.universe._
  def form_impl[ClassType:c.WeakTypeTag, FD:c.WeakTypeTag](caseClass:c.Expr[ClassType],
                                                               action:c.Expr[ClassType => Either[Exception, ClassType]])
                                                              (moduleDesc:c.Expr[FD]) = {

    val classExpr = caseClass
    
    val classWTT = implicitly[WeakTypeTag[ClassType]]

    val fieldsList = classWTT.tpe.typeSymbol.companion.typeSignature.members.
      find(_.name.toString == "apply").
      toList.
      flatMap(_.asMethod.paramLists.flatten)

    val i18nPrefix = classWTT.tpe.typeSymbol.name.toString.trim.dotNotation

    val (fieldsCode, fieldNamesList, gettersList, emptyValuesList) = fieldsList.
      collect{
      case field if field.annotations.map(_.tree.tpe).contains(weakTypeOf[field]) =>
        val (code, fieldName, emptyValue) = generateFieldBlock[FD, ClassType](
          q"$classExpr.${field.name.toTermName}",
          field,
          i18nPrefix,
          q"${i18nPrefix + "." + field.name.toString.trim.dotNotation}",
          classExpr)

        (code, q"$fieldName", field -> q"$fieldName.getValue", (field, fieldName, emptyValue))
    }.unzip4

    val getterExpressions = fieldsList.map(gettersList.toMap.withDefault(s => q"$classExpr.${s.name.toTermName}"))

    val (buttonNames, buttonCodes) = {
      val submitButton = {
        val buttonName = TermName(c.freshName())
        val i18nKey = s"$i18nPrefix.submit"

        buttonName -> q"""
          val $buttonName = {
            m.i18nKeyCollector.insert($i18nPrefix)($i18nKey)
            button($i18nKey, {() =>
              Option((fields.asInstanceOf[List[com.soulever.makro.BaseField[_, $classWTT]]] foldLeft true){ case (b, f) => f.isValid && b }).
              filter(identity).
              flatMap{ _ =>
                Option($classExpr.copy(..$getterExpressions)).
                filter(ob => (fields.asInstanceOf[List[com.soulever.makro.BaseField[_, $classWTT]]] foldLeft true){ case (b, f) => f.isValid(ob) && b })
              }.foreach($action)
            }, $fieldNamesList)
          }
       """
      }

      val resetButton = {
        val buttonName = TermName(c.freshName())
        val i18nKey = s"$i18nPrefix.reset"

        val emptyValueExpressionsList = emptyValuesList.collect {
          case (field, _, emptyValue) if !field.asTerm.isParamWithDefault => q"${field.name.toTermName} = $emptyValue"
        }

        val settersList = emptyValuesList.map{ case (field, fieldName, _) => q"$fieldName.setValue(empty.${field.name.toTermName})"}

        buttonName -> q"""
          val $buttonName = {
            m.i18nKeyCollector.insert($i18nPrefix)($i18nKey)
            button($i18nKey,{() =>
              val empty = new $classWTT(..$emptyValueExpressionsList)
              ..$settersList
            }, $fieldNamesList)
          }
       """
      }

      List(submitButton, resetButton)
    }.unzip(identity)

    val comp = q"""
    val m = $moduleDesc
      import m._
      ..${fieldsCode.flatten}
      val fields = $fieldNamesList
      ..$buttonCodes
      val form = m.form(fields, $buttonNames)
      m.i18nKeyCollector.print
      form
    """
    println(comp)
    comp
  }

  def field_impl[FieldType:c.WeakTypeTag, FD:c.WeakTypeTag, ClassType](value:c.Expr[FieldType],
                                                                       i18nKey:c.Expr[String],
                                                                       ths:c.Expr[ClassType])
                                                                      (moduleDesc:c.Expr[FD]) = {

    val (code, fieldName, _) = generateFieldBlock[FD, ClassType](value.tree, value.tree.symbol, "", i18nKey.tree, ths)

    val tree: Tree = q"""{
        val m = $moduleDesc
        import m._
       ..$code
       $fieldName
       }
      """
    println(tree)
    tree
  }

  def generateFieldBlock[FD:c.WeakTypeTag, ClassType: c.WeakTypeTag](valueTree: Tree,
                                                                     fieldSymbol: Symbol,
                                                                     i18nPrefix: String,
                                                                     i18nKey: Tree,
                                                                     ths:c.Expr[ClassType]) = {

    val valueType = fieldSymbol.typeSignature

    val fieldType = implicitly[WeakTypeTag[FD]].tpe.member(TypeName("FieldType")).asType.toType.dealias.typeConstructor

    val fieldName = TermName(c.freshName() + "Field")

    val css = Option(fieldSymbol).flatMap(_.annotations.collectFirst {
      case s if s.tree.tpe.typeConstructor =:= c.weakTypeOf[com.soulever.makro.css] => s.tree.children.tail.head
    })

    val (innerField, emptyValue) = {

      val mapping = Option(fieldSymbol).flatMap(_.annotations.collectFirst {
        case s if s.tree.tpe.typeConstructor =:= weakTypeOf[com.soulever.makro.mapping[Any, Any]].typeConstructor =>
          q"${s.tree.children.tail.head}(m)"
      })

      val fieldDescriptorType = {
        val tpe: Type = implicitly[WeakTypeTag[FD]].tpe
        val mdfClass: ClassSymbol = typeOf[MFieldDescriptor[_]].typeSymbol.asClass
        val selfType: Type = mdfClass.typeParams.head.asType.toType
        selfType.asSeenFrom(tpe, mdfClass)
      }

      def expandParameters(s: Type = valueType, collector: List[Tree] = List.empty): List[Tree] = {
        val (pre, args) = s match {
          case NullaryMethodType(TypeRef(a:Type, _, b)) =>
            a -> b
          case NullaryMethodType(a:Type) =>
            a -> List.empty
          case TypeRef(pre, _, args) =>
            pre -> args
          case _ => c.abort(c.enclosingPosition, "Cannot decode the position")
        }

        args match {
          case Nil if s <:< weakTypeOf[Enumeration#Value] =>
            q"enumFieldProvider[$pre](${pre.termSymbol})" :: collector
          case Nil =>
            q"implicitly[com.soulever.makro.TypeFieldProvider[$s, $fieldType, $fieldDescriptorType]]" :: collector
          case x :: Nil if s.typeConstructor =:= c.weakTypeOf[com.soulever.makro.types.Mapping[Any]].typeConstructor =>
            if (mapping.isEmpty) c.error(valueTree.pos, "Cannot find mapping for the given type")
            q"mappingFieldProvider[$x]($mapping.getOrElse(List.empty))" :: collector
          case x :: Nil =>
            expandParameters(x, q"implicitly[com.soulever.makro.KindFieldProvider[${s.finalResultType.typeConstructor}, $fieldType, $fieldDescriptorType]]" :: collector)
          case _ => q"implicitly[com.soulever.makro.TypeFieldProvider[$s, $fieldType, $fieldDescriptorType]]" :: collector
        }
      }

      val fieldProviders = expandParameters()
      (fieldProviders.tail foldLeft (q"${fieldProviders.head}.field(m)", q"${fieldProviders.head}.empty")){
        case ((underlyingFieldProvider, underlyingEmpty), thisFieldProvider) =>
          (q"$thisFieldProvider.field($underlyingFieldProvider, $underlyingEmpty, m)", q"$thisFieldProvider.empty")
      }
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

    (List(
      q"""
      val $fieldName = {
        m.i18nKeyCollector.insert($i18nPrefix)($i18nKey)
        ($validationMessages ::: $classDependentValidationMessages).foreach(m.i18nKeyCollector.insert($i18nPrefix))
        val field = m.field[$valueType, ${ths.actualType}]($valueTree, $i18nKey.trim, $innerField, $validators, $classDependentValidators, $css.getOrElse(""))
        field.asInstanceOf[com.soulever.makro.BaseField[_,_]].innerValidations.map(s => $i18nKey + "[" + s._1 + "]").foreach(m.i18nKeyCollector.insert($i18nPrefix))
        field.asInstanceOf[com.soulever.makro.BaseField[_,_]].innerI18nKeys.map(s => $i18nKey + "{" + s._1 + "}").foreach(m.i18nKeyCollector.insert($i18nPrefix))
        field
      }
      """), fieldName, emptyValue)
  }
}