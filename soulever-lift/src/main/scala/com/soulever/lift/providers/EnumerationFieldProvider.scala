package com.soulever.lift.providers

import com.soulever.lift.FieldDescriptor
import com.soulever.lift.types.{GeneratedField, InnerField, TypeFieldProvider}
import com.soulever.makro.AbstractFieldDescriptor
import com.soulever.makro.Soulever._
import net.liftweb.http.js.JsCmd

import scala.xml.NodeSeq

/**
 * @Auther tiran 
 * @Date 7/1/14.
 */
class EnumerationFieldProvider[A <: Enumeration](enum:A) extends TypeFieldProvider[A#Value] {

  override def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor: FD)
                                               (op: A#Value, baseField: GeneratedField[_, _]): InnerField[A#Value] =
    new InnerField[A#Value] {

      private val field: InnerField[Mapping[A#Value]] = new MappingFieldProvider(enum.values.toList.map(e => e.toString -> e.asInstanceOf[A#Value])).
        field(fieldDescriptor)(Mapping(op), baseField)

      override def value: A#Value = field.value

      override def elem: NodeSeq = field.elem

      override def validate: Either[String, A#Value] = field.validate.right.map(_.get)

      override def setValueWithJsCmd(value: A#Value): JsCmd = field.setValueWithJsCmd(value)
    }
}
