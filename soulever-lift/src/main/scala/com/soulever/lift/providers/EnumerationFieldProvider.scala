package com.soulever.lift.providers

import com.soulever.lift.FieldDescriptor
import com.soulever.lift.types.{GeneratedField, InnerField, TypeFieldProvider}
import com.soulever.makro.MFieldDescriptor
import com.soulever.makro.types.Mapping
import net.liftweb.http.js.JE.Value
import net.liftweb.http.js.JsCmd

import scala.xml.NodeSeq

/**
 * @Auther tiran 
 * @Date 7/1/14.
 */
class EnumerationFieldProvider[A <: Enumeration](enum:A) extends TypeFieldProvider[A#Value, FieldDescriptor] {

  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: A#Value, baseField: GeneratedField[_, _]): InnerField[A#Value] =
    new InnerField[A#Value] {

      private val field: InnerField[Mapping[A#Value]] = new MappingFieldProvider(enum.values.toList.map(e => e.toString -> e.asInstanceOf[A#Value])).
        field(fieldDescriptor)(Mapping(op), baseField)

      override def getValue: A#Value = field.getValue

      override def elem: NodeSeq = field.elem

      override def validate: Either[String, A#Value] = field.validate.right.map(_.get)

      override def setValueWithJsCmd(value: A#Value): JsCmd = field.setValueWithJsCmd(value)
    }

  override def empty: A#Value = enum.values.head
}