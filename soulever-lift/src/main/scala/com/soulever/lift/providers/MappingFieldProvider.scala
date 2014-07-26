package com.soulever.lift.providers

import com.soulever.lift.FieldDescriptor
import com.soulever.lift.types.{GeneratedField, InnerField, FieldProvider}
import com.soulever.makro.AbstractFieldDescriptor
import com.soulever.makro.Soulever._
import net.liftweb.common.{Full, Box}
import net.liftweb.http.{LiftRules, SHtml}
import net.liftweb.http.SHtml.SelectableOption
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.{Run, SetValById, ReplaceOptions}

import scala.xml.NodeSeq

/**
 * @Auther tiran 
 * @Date 6/30/14.
 */
class MappingFieldProvider[A](mapping:List[(String, A)]) extends FieldProvider[Mapping[A]] {
  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Mapping[A], baseField: GeneratedField[_, _]): InnerField[Mapping[A]] = {

    new InnerField[Mapping[A]] {

      val uniqueId = LiftRules.funcNameGenerator()

      var tempValue = op

      val options: List[SelectableOption[A]] = mapping.map { case (id, a) => SelectableOption(a, id, "id" -> s"$uniqueId-$id")}

      val field = {
        SHtml.ajaxSelectObj[A](options, Full(tempValue.get), { (a: A) =>
          tempValue = a
          JsCmd.unitToJsCmd();
        }, "id" -> uniqueId)
      }

      override def value: Mapping[A] = tempValue

      override def setValueWithJsCmd(v: Mapping[A]): JsCmd = {
        mapping.find(_._2 == v.get).map{ v =>
          println(v)
          tempValue = v._2
          println(options.collect{ case a if a.label == v._1 => a.attrs.toList})

          Run(
            s"""jQuery("#$uniqueId").
                val("$uniqueId-${options.collectFirst{ case a if a.label == v._1 => a.label}.getOrElse(options.head.label)}")
               """)
        }.getOrElse(JsCmd.unitToJsCmd())
      }

      override def elem: NodeSeq = field

      override def validate: Either[String, Mapping[A]] = Right(tempValue)
    }
  }
}
