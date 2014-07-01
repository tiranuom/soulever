package com.soulever.lift.providers

import com.soulever.lift.FieldDescriptor
import com.soulever.lift.types.{GeneratedField, InnerField, TypeFieldProvider}
import com.soulever.makro.MFieldDescriptor
import com.soulever.makro.types.Mapping
import net.liftweb.common.{Full, Box}
import net.liftweb.http.{LiftRules, SHtml}
import net.liftweb.http.SHtml.SelectableOption
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.{SetValById, ReplaceOptions}

import scala.xml.NodeSeq

/**
 * @Auther tiran 
 * @Date 6/30/14.
 */
class MappingFieldProvider[A](mapping:List[(String, A)]) extends TypeFieldProvider[Mapping[A], FieldDescriptor] {
  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Option[Mapping[A]], baseField: GeneratedField[_, _]): InnerField[Mapping[A]] = {

    new InnerField[Mapping[A]] {

      val uniqueId = LiftRules.funcNameGenerator()

      var value = op.getOrElse(empty)

      val field = SHtml.ajaxSelectObj[A](mapping.map{ case (id, a) => SelectableOption(a, id)}, Full(value.get), { (a:A) =>
        value = a
        JsCmd.unitToJsCmd();
      }, "id" -> uniqueId)

      override def getValue: Mapping[A] = value

      override def setValue(v: Mapping[A]): Unit = {
        mapping.find(_._2 == v).foreach{
          v =>
            value = v._2
            baseField.updateExpression(SetValById(uniqueId, v._1))
        }
      }

      override def elem: NodeSeq = field
    }
  }

  override def empty: Mapping[A] = new Mapping[A](mapping.head._2)
}
