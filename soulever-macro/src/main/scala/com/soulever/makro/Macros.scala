package com.soulever.makro

import java.io.PrintWriter

import com.soulever.makro.annotations._

import language.experimental.macros
import scala.annotation.tailrec
import scala.io.Source
import scala.reflect.macros.blackbox.Context

object Macros {
  /**
   * Generates a code for creting a UI form, for given case class,
   *  - fields in case class are transformed to ui fields.
   *  - two buttons for submit and reset are created.
   *
   * @param caseClass
   * @param action
   * @param moduleDesc
   * @tparam ClassType
   * @tparam FD
   * @return
   */
  def form[ClassType, FD <: MFieldDescriptor[_]](caseClass:ClassType, action:ClassType => Either[Exception, ClassType])
                                                (implicit moduleDesc:FD):FD#LayoutType= macro MacrosImpl.form_impl[ClassType, FD]

  /**
   * Generates a code for creating a UI field for given value
   *   - the value should be defined as a field and annotations should be given.
   *
   * @param value
   * @param i18nKey
   * @param ths
   * @param moduleDesc
   * @tparam FieldType
   * @tparam FD
   * @tparam ClassType
   * @return
   */
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

    if(Option(classExpr.tree.tpe.typeSymbol).filter(_.isClass).filter(_.asClass.isCaseClass).isEmpty){
      c.abort(c.enclosingPosition, "Only case classes are supported.")
    }

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
          q"$i18nPrefix",
          q"${field.name.toString.trim.dotNotation}",
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
            m.i18nKeyCollector.insert($i18nKey, "Submit")
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
            m.i18nKeyCollector.insert($i18nKey, "Reset")
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
      import com.soulever.makro._
      m.i18nKeyCollector.addBlock($i18nPrefix)
      ..${fieldsCode.flatten}
      val fields = $fieldNamesList
      ..$buttonCodes
      val form = m.form(fields, $buttonNames)
      m.i18nKeyCollector.print
      form
    """

    val writer: PrintWriter = new PrintWriter("/tmp/generated.scala")
    writer.write(showCode(comp))
    writer.close()
//    println(showCode(comp))
    comp
  }

  def field_impl[FieldType:c.WeakTypeTag, FD:c.WeakTypeTag, ClassType](value:c.Expr[FieldType],
                                                                       i18nKey:c.Expr[String],
                                                                       ths:c.Expr[ClassType])
                                                                      (moduleDesc:c.Expr[FD]) = {

    @tailrec
    def findClass(s:Symbol):Symbol = {
      if (s.isClass || s.isModule || s.isModuleClass || s.isPackage || s.isPackageClass) s
      else findClass(s.owner)
    }
    val className: String = findClass(c.internal.enclosingOwner).name.toString.dotNotation

    val (code, fieldName, _) = generateFieldBlock[FD, ClassType](value.tree, value.tree.symbol, q"$className", i18nKey.tree, ths)

    q"""{
        val m = $moduleDesc
        import m._
        m.i18nKeyCollector.addBlock($className)
       ..$code
       $fieldName
       }
      """
  }

  def generateFieldBlock[FD:c.WeakTypeTag, ClassType: c.WeakTypeTag](valueTree: Tree,
                                                                     fieldSymbol: Symbol,
                                                                     i18nPrefix: Tree,
                                                                     i18nKey: Tree,
                                                                     ths:c.Expr[ClassType]) = {

    val valueType = fieldSymbol.typeSignature

    val fieldType = implicitly[WeakTypeTag[FD]].tpe.member(TypeName("FieldType")).asType.toType.dealias.typeConstructor

    val fieldName = TermName(c.freshName() + "Field")

    val css = Option(fieldSymbol).flatMap(_.annotations.collectFirst {
      case s if s.tree.tpe.typeConstructor =:= c.weakTypeOf[css] => s.tree.children.tail.head
    })

    val (innerField, emptyValue) = {

      val mapping = Option(fieldSymbol).flatMap(_.annotations.collectFirst {
        case s if s.tree.tpe.typeConstructor =:= weakTypeOf[mapping[Any, Any]].typeConstructor =>
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
            q"implicitly[com.soulever.makro.providers.TypeFieldProvider[$s, $fieldType, $fieldDescriptorType]]" :: collector
          case x :: Nil if s.typeConstructor =:= c.weakTypeOf[com.soulever.makro.types.Mapping[Any]].typeConstructor =>
            if (mapping.isEmpty) c.error(fieldSymbol.pos, "Cannot find mapping for the given type")
            q"mappingFieldProvider[$x]($mapping.getOrElse(List.empty))" :: collector
          case x :: Nil =>
            expandParameters(x, q"implicitly[com.soulever.makro.providers.KindFieldProvider[${s.finalResultType.typeConstructor}, $fieldType, $fieldDescriptorType]]" :: collector)
          case _ => q"implicitly[com.soulever.makro.providers.TypeFieldProvider[$s, $fieldType, $fieldDescriptorType]]" :: collector
        }
      }

      val fieldProviders = expandParameters()
      (fieldProviders.tail foldLeft (q"${fieldProviders.head}.field(m)", q"${fieldProviders.head}.empty")){
        case ((underlyingFieldProvider, underlyingEmpty), thisFieldProvider) =>
          (q"$thisFieldProvider.field($underlyingFieldProvider, $underlyingEmpty, m)", q"$thisFieldProvider.empty")
      }
    }

    val (validations, validatorFunctions) = {
      val provided = Option(fieldSymbol).toList.flatMap(_.annotations.filter(_.tree.tpe <:< weakTypeOf[FieldValidation.AnnotationType]))
      provided.foreach(FieldValidation.validate[ClassType](c)(fieldSymbol))
      provided.map(FieldValidation.generateCodeBlock[ClassType](c)(fieldSymbol, q""" $i18nPrefix + "." + $i18nKey """))
    }.unzip(identity)

    val (classDependentValidations, classDependentValidatorFunctions) = {
      val provided = Option(fieldSymbol).toList.flatMap(_.annotations.filter(_.tree.tpe <:< weakTypeOf[FieldValidation2.AnnotationType]))
      provided.foreach(FieldValidation2.validate[ClassType](c)(fieldSymbol))
      provided.map(FieldValidation2.generateCodeBlock[ClassType](c)(fieldSymbol, q""" $i18nPrefix + "." + $i18nKey """))
    }.unzip(identity)

    val compoundI18nKey = q""" $i18nPrefix + "." + $i18nKey """

    (List(
      q"""
      val $fieldName = {
        m.i18nKeyCollector.insert($compoundI18nKey, $i18nKey.naturalNotation)
        ($validations ::: $classDependentValidations).foreach((v:com.soulever.makro.annotations.ValidationMessageProvider) => m.i18nKeyCollector.insert($compoundI18nKey + "[" + v.message + "]", v.defaultErrorMessage))
        val field = m.field[$valueType, ${ths.actualType}]($valueTree, ($compoundI18nKey).trim, $innerField, $validatorFunctions, $classDependentValidatorFunctions, $css.getOrElse(""))
        field.asInstanceOf[com.soulever.makro.BaseField[_,_]].innerValidations.foreach{ case (key, defaultValue) => m.i18nKeyCollector.insert($compoundI18nKey + "[" + key + "]", defaultValue)}
        field.asInstanceOf[com.soulever.makro.BaseField[_,_]].innerI18nKeys.foreach{ case (key, defaultValue) => m.i18nKeyCollector.insert($compoundI18nKey + "{" + key + "}", defaultValue)}
        field
      }
      """), fieldName, emptyValue)
  }
}