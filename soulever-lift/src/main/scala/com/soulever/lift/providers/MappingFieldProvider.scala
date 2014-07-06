package com.soulever.lift.providers

import com.soulever.lift.FieldDescriptor
import com.soulever.lift.types.{GeneratedField, InnerField, TypeFieldProvider}
import com.soulever.makro.MFieldDescriptor
import com.soulever.makro.types.Mapping
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
class MappingFieldProvider[A](mapping:List[(String, A)]) extends TypeFieldProvider[Mapping[A], FieldDescriptor] {
  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: Mapping[A], baseField: GeneratedField[_, _]): InnerField[Mapping[A]] = {

    new InnerField[Mapping[A]] {

      val uniqueId = LiftRules.funcNameGenerator()

      var value = op

      val options: List[SelectableOption[A]] = mapping.map { case (id, a) => SelectableOption(a, id, "id" -> s"$uniqueId-$id")}

      val field = {
        SHtml.ajaxSelectObj[A](options, Full(value.get), { (a: A) =>
          value = a
          JsCmd.unitToJsCmd();
        }, "id" -> uniqueId)
      }

      override def getValue: Mapping[A] = value

      override def setValueWithJsCmd(v: Mapping[A]): JsCmd = {
        mapping.find(_._2 == v.get).map{ v =>
          println(v)
          value = v._2
          println(options.collect{ case a if a.label == v._1 => a.attrs.toList})

          Run(
            s"""jQuery("#$uniqueId").
                val("$uniqueId-${options.collectFirst{ case a if a.label == v._1 => a.label}.getOrElse(options.head.label)}")
               """)
        }.getOrElse(JsCmd.unitToJsCmd())
      }

      override def elem: NodeSeq = field

      override def validate: Either[String, Mapping[A]] = Right(value)
    }
  }

  override def empty: Mapping[A] = new Mapping[A](mapping.head._2)
}