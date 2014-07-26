package com.soulever.lift.providers

import com.soulever.lift.FieldDescriptor
import com.soulever.lift.types.{FieldProvider, InnerField}
import com.soulever.makro.providers.EmptyProvider
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.Run
import net.liftweb.http.{LiftRules, SHtml}

import scala.xml.NodeSeq

class OptionFieldProvider[A : FieldProvider : EmptyProvider] extends FieldProvider[Option[A]] {
  override def field[FD <: FieldDescriptor](fieldDescriptor: FD)(op: Option[A],
                                                                            baseField: FD#BaseFieldType[_, _]): InnerField[Option[A]] =
    new InnerField[Option[A]] {

      var tempValue = op.isDefined
      var uniqueId = LiftRules.funcNameGenerator()

      def changeEnableState(b:Boolean) = {
        tempValue = b
        Run(s"""jQuery("input", "#${uniqueId + "-controlled"}").attr("disabled", ${if(tempValue) """ null """ else """ "disabled" """})""")
      }

      val iField = implicitly[FieldProvider[A]].field(fieldDescriptor)(op.getOrElse(implicitly[EmptyProvider[A]].empty), baseField)
      val checkbox = SHtml.ajaxCheckbox(true, changeEnableState _ , "id" -> uniqueId)

      override def value: Option[A] = if (tempValue) Option(iField.value) else None

      //      override def setValue(v: Option[B]): Unit = {
      //        v.foreach(iField.setValue)
      //        changeEnableDisable(v.isDefined)
      //      }

      override def setValueWithJsCmd(v: Option[A]): JsCmd = {
        val option: Option[JsCmd] = v.map(iField.setValueWithJsCmd)
        changeEnableState(option.isDefined)
        option.getOrElse(JsCmd.unitToJsCmd())
      }

      override def elem: NodeSeq = <span>{checkbox}<span id={uniqueId + "-controlled"}>{iField.elem}</span></span>

      override def innerI18nKeys: List[(String, String)] = iField.innerI18nKeys

      override def innerValidations: List[(String, String)] = iField.innerValidations

      override def validate: Either[String, Option[A]] = if (tempValue) iField.validate.right.map(Option.apply) else Right(None)
    }
}