package com.soulever.makro

import language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.{universe => ru}
import ru._
import java.io.{File, PrintWriter}

object Macros {
  def form[ClassType <: Product, FD <: MFieldDescriptor[Rt], Rt](init:ClassType, action:ClassType => Either[Exception, ClassType])
                                                                (implicit moduleDesc:FD):Rt = macro MacrosImpl.form_impl[ClassType, FD, Rt]

  def field[FieldType, FD <: MFieldDescriptor[_], ClassType](init:FieldType,
                                                             i18nKey:String,
                                                             validations:List[FieldValidation[FieldType]],
                                                             classDependentValidations:List[FieldValidation2[FieldType, ClassType]],
                                                             css:String,
                                                             ths:ClassType)
                                                            (implicit moduleDesc:FD):FD#BaseFieldType[FieldType, ClassType] = macro MacrosImpl.field_impl[FieldType, FD, ClassType]
}

class MacrosImpl(val c:Context) {
  import c.universe._
  def form_impl[ClassType:c.WeakTypeTag, FD:c.WeakTypeTag, Rt](init:c.Expr[ClassType], action:c.Expr[ClassType => Either[Exception, ClassType]])
                                                              (moduleDesc:c.Expr[FD]) = {


    val initWtt = implicitly[WeakTypeTag[ClassType]]

    val beanTpe = initWtt.tpe

    val fields = beanTpe.typeSymbol.companion.typeSignature.members.collectFirst {
      case method if method.name.toString == "apply" => method
    }.toList.flatMap(_.asMethod.paramLists.flatten)

    val formFields = for {
      field <- fields if field.annotations.map(_.tree.tpe).contains(weakTypeOf[field])
    } yield field

    val fieldExpansionData = formFields.map { field =>
      fieldExpansion[ClassType, FD](q"$init.${field.name.toTermName}", field, initWtt.tpe.typeSymbol.pos, toDotNotation(initWtt.tpe.typeSymbol.name.toString.trim), init)
    }

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

    val submitButtonKey = toDotNotation(beanTpe.typeSymbol.name.toString) + ".submit"

    val comp = q"""
    val m = $moduleDesc
      import m._
      ..$fieldExpressions
      val fields = $fieldListExpression
      I18nKeyCollector.insert(${toDotNotation(beanTpe.typeSymbol.name.toString)})($submitButtonKey)
      val $submitButtonName = submitButton($submitButtonKey, {() =>
        Option((fields.asInstanceOf[List[com.soulever.makro.BaseField[_, $initWtt]]] foldLeft true){ case (b, f) => f.isValid && b }).
          filter(identity).
          flatMap{ _ =>
            Option($init.copy(..$copyParams)).
              filter(ob => (fields.asInstanceOf[List[com.soulever.makro.BaseField[_, $initWtt]]] foldLeft true){ case (b, f) => f.isValid(ob) && b })
          }.foreach($action)
      })
      form(fields, List($submitButtonName))
    """
    val file = new File("/tmp/test.txt")
    val writer = new PrintWriter(file)
    writer.write(show(comp))
    writer.close()
//        println( s"""comp = ${show(comp)} """)

    comp
  }

  def toDotNotation(s:String) = s.replaceAll(
    String.format("%s|%s|%s", "(?<=[A-Z])(?=[A-Z][a-z])", "(?<=[^A-Z])(?=[A-Z])", "(?<=[A-Za-z])(?=[^A-Za-z])"),
    "."
  ).toLowerCase.trim

  def field_impl[FieldType:c.WeakTypeTag, FD:c.WeakTypeTag, ClassType](init:c.Expr[FieldType],
                                                                       i18nKey:c.Expr[String],
                                                                       validations:c.Expr[List[FieldValidation[FieldType]]],
                                                                       classDependentValidations:c.Expr[List[FieldValidation2[FieldType, ClassType]]],
                                                                       css:c.Expr[String],
                                                                       ths:c.Expr[ClassType])
                                                                      (moduleDesc:c.Expr[FD]) = {

//    val (fieldName, _, code) = fieldExpansion[ClassType, FD](init.tree, init.tree.symbol, init.tree.pos, "test", ths)

    val fieldImplType = implicitly[WeakTypeTag[FD]].tpe.member(TypeName("FieldType")).asType.toType.dealias.typeConstructor

    val innerField = {

      val mapping = Option(init.tree.symbol).flatMap(_.annotations.collectFirst {
        case s if s.tree.tpe.typeConstructor =:= weakTypeOf[com.soulever.makro.mapping[Any, Any]].typeConstructor =>
          q"${s.tree.children.tail.head}(m)"  //this is broken
      })

      def expandParameters(s: Type, collector: List[Tree] = List.empty): List[Tree] = {

        val TypeRef(pre, _, args) = s
        args match {
          case Nil if s <:< weakTypeOf[Enumeration#Value] =>
            q"enumFieldProvider[$pre](${pre.termSymbol})" :: collector
          case Nil =>
            q"implicitly[com.soulever.makro.TypeFieldProvider[$s, $fieldImplType]]" :: collector
          case x :: Nil if s.typeConstructor =:= c.weakTypeOf[com.soulever.makro.types.Mapping[Any]].typeConstructor =>
            q"mappingFieldProvider[$x]($mapping.getOrElse(List.empty))" :: collector
          case x :: Nil =>
            expandParameters(x, q"implicitly[com.soulever.makro.KindFieldProvider[${s.typeConstructor}, $fieldImplType]]" :: collector)
          case _ => q"implicitly[com.soulever.makro.TypeFieldProvider[$s, $fieldImplType]]" :: collector
        }
      }

      val types = expandParameters(init.actualType)
      (types.tail foldLeft(q"${types.head}.field(m, $i18nKey)", q"${types.head}.empty")) {
        case (quo, tpe) =>
          (q"$tpe.field(${quo._1}, ${quo._2})(m, $i18nKey)", q"$tpe.empty")
      }._1
    }

    val fieldName = TermName(c.freshName() + "Field")

    val ftt: WeakTypeTag[FieldType] = implicitly[WeakTypeTag[FieldType]]

    val validationMessages: Tree = q"$validations.map((_:FieldValidation[$ftt]).message)"
    val classDependentValidaitonMessages: Tree = q"$classDependentValidations.map((_:FieldValidation2[$ftt, ${ths.actualType}]).message)"
    val validatorFuncs: Tree = q"""
        $validations.map { (validator:FieldValidation[$ftt]) =>
        (x:$ftt) =>
        Option(x).filter(validator.validate).toRight($i18nKey + s"[$${validator.message}]")
       }"""
    val classDependentValidatorFuncs: Tree = q"""
        $classDependentValidations.map { (validator:FieldValidation2[$ftt, ${ths.actualType}]) =>
        I18nKeyCollector.insert($i18nKey)($i18nKey + s"[$${validator.message}]")
        (x:$ftt, ths:${ths.actualType}) => Option(x).filter(a => validator.validate(a, ths)).toRight($i18nKey + s"[$${validator.message}]")
       }"""

    val code = List(
      q"""
      val $fieldName = {
        I18nKeyCollector.insert()($i18nKey)
        val field = m.field[$ftt, ${ths.actualType}]($init, $i18nKey.trim, $innerField, $validatorFuncs, $classDependentValidatorFuncs, $css)
        ($validationMessages ::: $classDependentValidaitonMessages).filter(_ != null).foreach(I18nKeyCollector.insert($i18nKey))
        field.asInstanceOf[com.soulever.makro.BaseField[_,_]].innerValidations.map(s => $i18nKey + "[" + s + "]").foreach(I18nKeyCollector.insert($i18nKey))
        field.asInstanceOf[com.soulever.makro.BaseField[_,_]].innerI18nKeys.map(s => $i18nKey + "{" + s + "}").foreach(I18nKeyCollector.insert($i18nKey))
        field
      }
      """)

    val comp: Tree = q"""{
        val m = $moduleDesc
        import m._
       ..$code
       $fieldName
       }
      """

    comp
    //    q"null"
  }

  def fieldExpansion[ClassType :c.WeakTypeTag, FD:c.WeakTypeTag](value:Tree, field:Symbol, position:Position, i18nPrefix: String, ths:c.Expr[ClassType]) = { //init:Option[c.Expr[ClassType]]
  val fieldImplType = implicitly[WeakTypeTag[FD]].tpe.member(TypeName("FieldType")).asType.toType.dealias.typeConstructor

    val fieldName = TermName(c.freshName() + "Field")

    val css = field.annotations.collectFirst {
      case s if s.tree.tpe.typeConstructor == c.weakTypeOf[com.soulever.makro.css] => s.tree.children.tail.head
    }

    val i18nKey = q"${i18nPrefix + "." + toDotNotation(field.name.toString.trim)}"

    val innerField = {

      val mapping = field.annotations.collectFirst {
        case s if s.tree.tpe.typeConstructor =:= weakTypeOf[com.soulever.makro.mapping[Any, Any]].typeConstructor =>
          q"${s.tree.children.tail.head}(m)"  //this is broken
      }

      def expandParameters(s: Type, collector: List[Tree] = List.empty): List[Tree] = {
        val TypeRef(pre, _, args) = s
        args match {
          case Nil if s <:< weakTypeOf[Enumeration#Value] =>
            q"enumFieldProvider[$pre](${pre.termSymbol})" :: collector
          case Nil =>
            q"implicitly[com.soulever.makro.TypeFieldProvider[$s, $fieldImplType]]" :: collector
          case x :: Nil if s.typeConstructor =:= c.weakTypeOf[com.soulever.makro.types.Mapping[Any]].typeConstructor =>
            if (mapping.isEmpty) c.error(position, "Cannot find mapping for the given type")
            q"mappingFieldProvider[$x](${mapping}.getOrElse(List.empty))" :: collector
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


    val (validationMessages, validators) = {
      val provided = field.annotations.filter(_.tree.tpe <:< weakTypeOf[FieldValidation.AnnotationType])
      provided.foreach(FieldValidation.validate[ClassType](c)(field))
      provided.map(FieldValidation.generateCodeBlock[ClassType](c)(field, i18nKey))
    }.unzip(identity)

    val (classDependentValidationMessages, classDependentValidators) = {
      val provided = field.annotations.filter(_.tree.tpe <:< weakTypeOf[FieldValidation2.AnnotationType])
      provided.foreach(FieldValidation2.validate[ClassType](c)(field))
      provided.map(FieldValidation2.generateCodeBlock[ClassType](c)(field, i18nKey))
    }.unzip(identity)
    //List(..${validations.map(_._1)}, ..${validations2.map(_._1)}).foreach(I18nKeyCollector.insert($i18nPrefix))

    (fieldName, field, List(
      q"""
      val $fieldName = {
        I18nKeyCollector.insert($i18nPrefix)($i18nKey)
        val field = m.field($value, $i18nKey.trim, $innerField, List(..$validators), List(..$classDependentValidators), ${css.getOrElse(q""" "" """)})
        List(..$validationMessages, ..$classDependentValidationMessages).foreach(I18nKeyCollector.insert($i18nPrefix))
        field.asInstanceOf[com.soulever.makro.BaseField[_,_]].innerValidations.map(s => $i18nKey + "[" + s._1 + "]").foreach(I18nKeyCollector.insert($i18nPrefix))
        field.asInstanceOf[com.soulever.makro.BaseField[_,_]].innerI18nKeys.map(s => $i18nKey + "{" + s._1 + "}").foreach(I18nKeyCollector.insert($i18nPrefix))
        field
      }
      """))
  }
}