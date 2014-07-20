package com.soulever.lift.providers

import com.soulever.lift.FieldDescriptor
import com.soulever.lift.types.{GeneratedField, InnerField, KindFieldProvider}
import com.soulever.makro.AbstractFieldDescriptor
import net.liftweb.http.js.{JsCmd, JsExp}
import net.liftweb.http.js.jquery.JqJsCmds.{JqSetHtml, AppendHtml}
import net.liftweb.http.{LiftRules, SHtml}
import net.liftweb.http.js.jquery.JqJE.{JqAppend, JqRemove, JqId}

import scala.xml.{Elem, NodeSeq}

/**
 * @Auther tiran 
 * @Date 7/1/14.
 */
class ListKindFieldProvider extends KindFieldProvider[List, FieldDescriptor]{
  override def field[B, FD <: AbstractFieldDescriptor[_]](innerField: (B, GeneratedField[_, _]) => InnerField[B],
                                                   innerEmpty: B,
                                                   fieldDescriptor: FD)
                                                  (op: List[B],
                                                   baseField: GeneratedField[_, _]): InnerField[List[B]] = new InnerField[List[B]] {

    def createField(value:B):(String, InnerField[B], Elem) = {
      val fieldId = LiftRules.funcNameGenerator()
      val field: InnerField[B] = innerField(value, baseField)
      val remove = SHtml.ajaxButton("-", () => {
        fieldsList = fieldsList.filterNot(_._1 == fieldId)
        JqId(fieldId).~>(JqRemove()).cmd
      })
      (fieldId, field, <li style="list-style: none;" id={fieldId}>{field.elem}{remove}</li>)
    }

    val listId = LiftRules.funcNameGenerator.apply()

    val addButton = SHtml.ajaxButton("+", () => {
      val innerField = createField(innerEmpty)
      fieldsList = fieldsList ::: List(innerField)
      AppendHtml(listId, innerField._3)
    })

    var fieldsList = op.map(createField)

    override def value: List[B] = fieldsList.map{ case (_, field, _) => field.value }

    override def setValueWithJsCmd(value: List[B]): JsCmd = {
      fieldsList = value.map(createField)
      JqSetHtml(listId, {fieldsList.map(_._3)})
    }

    override def validate: Either[String, List[B]] = fieldsList.
      foldLeft(Right(List.empty):Either[String, List[B]]) {
      case (res, (_, field, _)) => res.right.flatMap(list => field.validate.right.map(list ::: List(_)))
    }

    override def elem: NodeSeq = <div><ul id={listId}>{fieldsList.map(_._3)}</ul><div>{addButton}</div></div>
  }
}
