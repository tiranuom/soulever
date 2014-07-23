package com.soulever.lift.providers

import com.soulever.lift.FieldDescriptor
import com.soulever.lift.types.{TypeFieldProvider, GeneratedField, InnerField, KindFieldProvider}
import com.soulever.makro.AbstractFieldDescriptor
import net.liftweb.http.js.JsCmd
import net.liftweb.http.{LiftRules, SHtml}
import net.liftweb.http.js.JsCmds.{Run, SetValById}

import scala.xml.NodeSeq

/**
 * @Auther tiran 
 * @Date 7/1/14.
 */
class OptionKindFieldProvider extends KindFieldProvider[Option, FieldDescriptor] {
  override def field[B, FD <: AbstractFieldDescriptor[_]](innerField: (B, GeneratedField[_, _]) => InnerField[B], innerEmpty: B, fieldDescriptor: FD)
                                                  (op: Option[B], baseField: GeneratedField[_, _]): InnerField[Option[B]] =
    new InnerField[Option[B]] {

      var tempValue = op.isDefined
      var uniqueId = LiftRules.funcNameGenerator()

      def changeEnableState(b:Boolean) = {
        tempValue = b
        Run(s"""jQuery("input", "#${uniqueId + "-controlled"}").attr("disabled", ${if(tempValue) """ null """ else """ "disabled" """})""")
      }

      val iField = innerField(op.getOrElse(innerEmpty), baseField)
      val checkbox = SHtml.ajaxCheckbox(true, changeEnableState _ , "id" -> uniqueId)

      override def value: Option[B] = if (tempValue) Option(iField.value) else None

//      override def setValue(v: Option[B]): Unit = {
//        v.foreach(iField.setValue)
//        changeEnableDisable(v.isDefined)
//      }

      override def setValueWithJsCmd(v: Option[B]): JsCmd = {
        val option: Option[JsCmd] = v.map(iField.setValueWithJsCmd)
        changeEnableState(option.isDefined)
        option.getOrElse(JsCmd.unitToJsCmd())
      }

      override def elem: NodeSeq = <span>{checkbox}<span id={uniqueId + "-controlled"}>{iField.elem}</span></span>

      override def innerI18nKeys: List[(String, String)] = iField.innerI18nKeys

      override def innerValidations: List[(String, String)] = iField.innerValidations

      override def validate: Either[String, Option[B]] = if (tempValue) iField.validate.right.map(Option.apply) else Right(None)
    }
}

//class OptionFieldProvider[A](implicit innerField:TypeFieldProvider[A, FieldDescriptor]) extends TypeFieldProvider[Option[A], FieldDescriptor] {
//  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)(op: Option[A],
//                                                                            baseField: FieldDescriptor#BaseFieldType[_, _]): InnerField[Option[A]] =
//    new InnerField[Option[A]] {
//
//      var tempValue = op.isDefined
//      var uniqueId = LiftRules.funcNameGenerator()
//
//      def changeEnableState(b:Boolean) = {
//        tempValue = b
//        Run(s"""jQuery("input", "#${uniqueId + "-controlled"}").attr("disabled", ${if(tempValue) """ null """ else """ "disabled" """})""")
//      }
//
//      val iField = innerField(op.getOrElse(innerEmpty), baseField)
//      val checkbox = SHtml.ajaxCheckbox(true, changeEnableState _ , "id" -> uniqueId)
//
//      override def value: Option[A] = if (tempValue) Option(iField.value) else None
//
//      //      override def setValue(v: Option[B]): Unit = {
//      //        v.foreach(iField.setValue)
//      //        changeEnableDisable(v.isDefined)
//      //      }
//
//      override def setValueWithJsCmd(v: Option[A]): JsCmd = {
//        val option: Option[JsCmd] = v.map(iField.setValueWithJsCmd)
//        changeEnableState(option.isDefined)
//        option.getOrElse(JsCmd.unitToJsCmd())
//      }
//
//      override def elem: NodeSeq = <span>{checkbox}<span id={uniqueId + "-controlled"}>{iField.elem}</span></span>
//
//      override def innerI18nKeys: List[(String, String)] = iField.innerI18nKeys
//
//      override def innerValidations: List[(String, String)] = iField.innerValidations
//
//      override def validate: Either[String, Option[B]] = if (tempValue) iField.validate.right.map(Option.apply) else Right(None)
//    }
//}