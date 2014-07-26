package com.soulever.lift.providers

import com.soulever.lift.FieldDescriptor
import com.soulever.lift.types.{FieldProvider, InnerField}
import com.soulever.makro.providers.EmptyProvider
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.jquery.JqJE.{JqId, JqRemove}
import net.liftweb.http.js.jquery.JqJsCmds.{AppendHtml, JqSetHtml}
import net.liftweb.http.{LiftRules, SHtml}

import scala.xml.{Elem, NodeSeq}

class ListFieldProvider[A : FieldProvider : EmptyProvider] extends FieldProvider[List[A]]{
  override def field[FD <: FieldDescriptor](fieldDescriptor: FD)
                                           (op: List[A], baseField: FD#BaseFieldType[_, _]): InnerField[List[A]] =
    new InnerField[List[A]] {

      def createField(value:A):(String, InnerField[A], Elem) = {
        val fieldId = LiftRules.funcNameGenerator()
        val field: InnerField[A] = implicitly[FieldProvider[A]].field(fieldDescriptor)(value, baseField)
        val remove = SHtml.ajaxButton("-", () => {
          fieldsList = fieldsList.filterNot(_._1 == fieldId)
          JqId(fieldId).~>(JqRemove()).cmd
        })
        (fieldId, field, <li style="list-style: none;" id={fieldId}>{field.elem}{remove}</li>)
      }

      val listId = LiftRules.funcNameGenerator.apply()

      var fieldsList = op.map(createField)

      val addButton = SHtml.ajaxButton("+", () => {
        val innerField = createField(implicitly[EmptyProvider[A]].empty)
        fieldsList = fieldsList ::: List(innerField)
        AppendHtml(listId, innerField._3)
      })

      override def value: List[A] = fieldsList.map{ case (_, field, _) => field.value }

      override def setValueWithJsCmd(value: List[A]): JsCmd = {
        fieldsList = value.map(createField)
        JqSetHtml(listId, {fieldsList.map(_._3)})
      }

      override def validate: Either[String, List[A]] = fieldsList.
        foldLeft(Right(List.empty):Either[String, List[A]]) {
        case (res, (_, field, _)) => res.right.flatMap(list => field.validate.right.map(list ::: List(_)))
      }

      override def elem: NodeSeq = <div><ul id={listId}>{fieldsList.map(_._3)}</ul><div>{addButton}</div></div>
    }
}