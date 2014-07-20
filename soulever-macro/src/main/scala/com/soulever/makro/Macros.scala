package com.soulever.makro

import java.io.PrintWriter

import com.soulever.makro.annotations._
import com.soulever.makro.form.ButtonBlock

import language.experimental.macros
import scala.reflect.macros.blackbox
import Soulever._

object Macros {
  /**
   * Generates a code for creting a UI form, for given case class,
   *  - fields in case class are transformed to ui fields.
   *  - two buttons for submit and reset are created.
   *
   * @param instance
   * @param action
   * @param moduleDesc
   * @tparam ClassType
   * @tparam FD
   * @return
   */
  def form[ClassType, FD <: AbstractFieldDescriptor[_]](instance:ClassType, buttons:List[ButtonBlock[ClassType]])
                                                       (implicit moduleDesc:FD):FD#LayoutType= macro MacrosImpl.form_impl[ClassType, FD]

  /**
   * Generates a code for creating a UI field for given value type
   *
   * @param value value should be used as the initial value. should be defined as a val and define the annotations.
   * @param i18nKey i18n key to be used.
   * @param ths
   * @param moduleDesc
   * @tparam FieldType
   * @tparam FD
   * @tparam ClassType
   * @return
   */
  def field[FieldType, FD <: AbstractFieldDescriptor[_], ClassType](value:FieldType,
                                                                    i18nKey:String,
                                                                    ths:ClassType)
                                                                   (implicit moduleDesc:FD):FD#BaseFieldType[FieldType, ClassType] = macro MacrosImpl.field_impl[FieldType, FD, ClassType]


}

class MacrosImpl(val c:blackbox.Context) {
  import c.universe._

  def form_impl[ClassType:c.WeakTypeTag, FD:c.WeakTypeTag](instance:c.Expr[ClassType],
                                                           buttons:c.Expr[List[ButtonBlock[ClassType]]])
                                                          (moduleDesc:c.Expr[FD]) = {

    if(Option(instance.tree.tpe.typeSymbol).filter(_.isClass).filter(_.asClass.isCaseClass).isEmpty){
      c.abort(c.enclosingPosition, "Only case classes are supported.")
    }

    val classExpr = instance

    val classTag = c.weakTypeOf[ClassType]

    val classI18nKey = classTag.typeSymbol.name.toString.trim.dotNotation

    val classParamsList = classTag.typeSymbol.companion.typeSignature.members.
      find(_.name.toString == "apply").
      toList.
      flatMap(_.asMethod.paramLists.flatten)

    val fieldDataList = for {
      field <- classParamsList if !field.annotations.map(_.tree.tpe).contains(weakTypeOf[hidden])
    } yield generateFieldBlock[FD, ClassType](
        valueTree   = q"$classExpr.${field.name.toTermName}",
        fieldSymbol = field,
        classI18nKey= q"$classI18nKey",
        fieldI18nKey= q"${field.name.toString.trim.dotNotation}",
        ths         = classExpr)

    val fieldCodeBlocks = fieldDataList.map(_.code)
    val fieldNamesList = fieldDataList.map(a => q"${a.fieldName}")
    val gettersList = fieldDataList.map(a => a.field -> q"${a.fieldName}.value")
    val emptyValuesList = fieldDataList.map(a => (a.field, a.fieldName, a.empty))

    val getterExpressions = classParamsList.map(gettersList.toMap.withDefault(s => q"$classExpr.${s.name.toTermName}"))

    val buttonCode = {
      val fieldDescriptorType = {
        val tpe = c.weakTypeOf[FD]
        val mdfClass = typeOf[AbstractFieldDescriptor[_]].typeSymbol.asClass
        val selfType = mdfClass.typeParams.head.asType.toType
        selfType.asSeenFrom(tpe, mdfClass)
      }

      //TODO After introducing empty providers, handle hidden fields without default values

      val noDefaultParamList = for {
        (field, _, emptyValue) <- emptyValuesList if !field.asTerm.isParamWithDefault
      } yield q"${field.name.toTermName} = $emptyValue"

      val settersList = for {
        (field, fieldName, _) <- emptyValuesList
      } yield q"$fieldName.value = empty.${field.name.toTermName}"

      val i18nKey = s"$classI18nKey.button"

      q"""
            $buttons.zipWithIndex.map { case (btn:ButtonBlock[${classTag.typeSymbol.name}], index:Int) =>
              val empty = new $classTag(..$noDefaultParamList)
              m.i18nKeyCollector.insert($i18nKey + s".$$index", btn.defaultI18n)
              btn.button[$fieldDescriptorType, ${c.weakTypeOf[FD]}](
                label = $i18nKey + s".$$index",
                fieldsList = fields,
                createInstance = _ => $classExpr.copy(..$getterExpressions),
                setEmpties = () => {
                  ..$settersList
                }
              )
            }
         """
    }

    val comp = q"""
    val m = $moduleDesc
      import m._
      import com.soulever.makro._
      m.i18nKeyCollector.addBlock($classI18nKey)
      ..$fieldCodeBlocks
      val fields = $fieldNamesList
      val formOb = m.formComponent(fields, $buttonCode)
      m.i18nKeyCollector.print
      formOb
    """

    val writer: PrintWriter = new PrintWriter("/tmp/generated.scala")
    writer.write(showCode(comp))
    writer.close()
    comp
  }

  def field_impl[FieldType:c.WeakTypeTag, FD:c.WeakTypeTag, ClassType](value:c.Expr[FieldType],
                                                                       i18nKey:c.Expr[String],
                                                                       ths:c.Expr[ClassType])
                                                                      (moduleDesc:c.Expr[FD]) = {

    val className: String = ths.tree.tpe.typeSymbol.name.toString.dotNotation

    val FieldData(code, fieldName, _, _) = generateFieldBlock[FD, ClassType](
      valueTree   = value.tree,
      fieldSymbol = value.tree.symbol,
      classI18nKey= q"$className",
      fieldI18nKey= i18nKey.tree,
      ths         = ths)

    q"""{
        val m = $moduleDesc
        import m._
        m.i18nKeyCollector.addBlock($className)
        $code
        $fieldName
      }
      """
  }

  def generateFieldBlock[FD:c.WeakTypeTag, ClassType: c.WeakTypeTag](valueTree: Tree,
                                                                     fieldSymbol: Symbol,
                                                                     classI18nKey: Tree,
                                                                     fieldI18nKey: Tree,
                                                                     ths:c.Expr[ClassType]):FieldData = {

    val valueType = fieldSymbol.typeSignature

    val fieldType = c.weakTypeOf[FD].member(TypeName("FieldType")).asType.toType.dealias.typeConstructor

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
        val tpe = c.weakTypeOf[FD]
        val mdfClass = typeOf[AbstractFieldDescriptor[_]].typeSymbol.asClass
        val selfType = mdfClass.typeParams.head.asType.toType
        selfType.asSeenFrom(tpe, mdfClass)
      }

      def expandParameters(s: Type = valueType, collector: List[(Tree, Tree)] = List.empty): List[(Tree, Tree)] = {
        val (pre, args) = s match {
          case NullaryMethodType(TypeRef(a:Type, _, b)) => a -> b
          case NullaryMethodType(a:Type) => a -> List.empty
          case TypeRef(a, _, b) => a -> b
          case _ => c.abort(c.enclosingPosition, "Cannot decode the position")
        }

        args match {
          case Nil if s <:< weakTypeOf[Enumeration#Value] =>
            (q"enumFieldProvider[$pre](${pre.termSymbol})", emptyBlock(fieldSymbol, s)) :: collector
          case Nil =>
            (q"implicitly[providers.TypeFieldProvider[$s, $fieldType, $fieldDescriptorType]]", emptyBlock(fieldSymbol, s)) ::
              collector
          case x :: Nil if s.typeConstructor =:= c.weakTypeOf[Mapping[Any]].typeConstructor =>
            if (mapping.isEmpty) c.error(fieldSymbol.pos, "Cannot find mapping for the given type")
            (q"mappingFieldProvider[$x]($mapping.getOrElse(List.empty))", emptyBlock(fieldSymbol, s)) ::
              collector
          case x :: Nil =>
            val providerTypes =
              (q"implicitly[providers.KindFieldProvider[${s.finalResultType.typeConstructor}, $fieldType, $fieldDescriptorType]]",
                emptyBlock(fieldSymbol, s)) ::
                collector
            expandParameters(x, providerTypes)
          case _ =>
            (q"implicitly[providers.TypeFieldProvider[$s, $fieldType, $fieldDescriptorType]]", emptyBlock(fieldSymbol, s)) ::
            collector
        }
      }

      val fieldProviders = expandParameters()
      (fieldProviders.tail foldLeft (q"${fieldProviders.head._1}.field(m)", q"${fieldProviders.head._2}.empty")){
        case ((underlyingFieldProvider, underlyingEmpty), thisFieldProvider) =>
          (q"${thisFieldProvider._1}.field($underlyingFieldProvider, $underlyingEmpty, m)", q"${thisFieldProvider._2}.empty")
      }
    }

    def createValidations(blockProvider: FieldBlockProvider) = {

      val provided = for {
        fieldSymbol <- Option(fieldSymbol).toList
        annotation <- fieldSymbol.annotations
        if annotation.tree.tpe <:< weakTypeOf[blockProvider.AnnotationType]
      } yield annotation

      provided.foreach(blockProvider.validate[ClassType](c)(fieldSymbol))

      provided.map(blockProvider.generateCodeBlock[ClassType](c)(fieldSymbol, q""" $classI18nKey + "." + $fieldI18nKey """))
    }.unzip(identity)

    val (validations, validatorFunctions) = createValidations(FieldValidation)

    val (classDependentValidations, classDependentValidatorFunctions) = createValidations(FieldValidation2)

    val compoundI18nKey = q""" $classI18nKey + "." + $fieldI18nKey """

    val code: Tree = q"""
      val $fieldName = {
        import com.soulever.makro._
        m.i18nKeyCollector.insert($compoundI18nKey, $fieldI18nKey.naturalNotation)

        val field = m.fieldComponent[$valueType, ${ths.actualType}](
          init                = $valueTree,
          caption             = ($compoundI18nKey).trim,
          innerField          = $innerField,
          validators          = $validatorFunctions,
          secondaryValidators = $classDependentValidatorFunctions,
          css                 = $css.getOrElse(""))

        (field.innerValidations ::: $validations ::: $classDependentValidations) foreach { case (key, defaultValue) =>
          m.i18nKeyCollector.insert($compoundI18nKey + s"[$$key]", defaultValue)
        }

        field.innerI18nKeys.foreach{ case (key, defaultValue) =>
          m.i18nKeyCollector.insert($compoundI18nKey + s"{$$key}", defaultValue)
        }

        field
      }

      """
    FieldData(code = code,
      fieldName = fieldName,
      empty = emptyValue,
      field = fieldSymbol)
  }

  def emptyBlock(fieldSymbol:Symbol, valueType:Type) = {
    val (pre, args) = valueType match {
      case NullaryMethodType(TypeRef(a:Type, _, b)) => a -> b
      case NullaryMethodType(a:Type) => a -> List.empty
      case TypeRef(a, _, b) => a -> b
      case _ => valueType -> List.empty
    }

    args match {
      case x :: Nil if valueType.typeConstructor =:= c.weakTypeOf[Mapping[Any]].typeConstructor =>
        val mapping = Option(fieldSymbol).flatMap(_.annotations.collectFirst {
          case s if s.tree.tpe.typeConstructor =:= weakTypeOf[mapping[Any, Any]].typeConstructor =>
            q"${s.tree.children.tail.head}(m)"
        })
        if (mapping.isEmpty) c.error(fieldSymbol.pos, "Cannot find mapping for the given type")
        q"mappingEmptyProvider($mapping.getOrElse(List.empty))"
      case Nil if valueType <:< weakTypeOf[Enumeration#Value] =>
        q"implicitly[providers.TypeEmptyProvider[$valueType]]"
      case Nil =>
        q"implicitly[providers.TypeEmptyProvider[$valueType]]"
      case x :: Nil =>
        q"implicitly[providers.KindEmptyProvider[${valueType.finalResultType.typeConstructor}]]"
      case _ => q"implicitly[providers.TypeEmptyProvider[$pre]]"
    }
  }

  case class FieldData(code:Tree, fieldName:TermName, empty:Tree, field:Symbol) {
    def fieldNameAsTree = q"$fieldName"

    def getter = q"$fieldName.value"
  }
}
