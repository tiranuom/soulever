package com.soulever.vaadin.providers

import com.soulever.vaadin.{TypeFieldProvider, KindFieldProvider}
import com.soulever.makro.MFieldDescriptor
import com.vaadin.ui._
import com.soulever.makro.types.Mapping

//class MappingFieldProvider1 extends KindFieldProvider[Mapping]{
//  def field[B, FD <: MFieldDescriptor[_]](inf: (Option[B]) => AbstractField[B])
//                                         (implicit fieldDescriptor: FD, mapping: Option[List[(String, B)]]):
//  (Option[Mapping[B]]) => AbstractField[Mapping[B]] = {
//    value =>
//      class MappingField extends CustomField[Mapping[B]] {
//        def getType: Class[_ <: Mapping[B]] = classOf[Mapping[B]]
//
//        val innerField: ComboBox = new ComboBox()
//        mapping.foreach(_.foreach{
//          case (n, v) =>
//            innerField.addItem(v)
//            innerField.setItemCaption(v, n)
//        })
//        value.foreach(innerField.setValue)
//
//        def initContent(): Component = innerField
//
//        override def setValue(newFieldValue: Mapping[B]) = innerField.setValue(newFieldValue.asInstanceOf[Mapping[B]])
////
//        override def getValue(): Mapping[B] = innerField.getValue.asInstanceOf[B]
//      }
//      new MappingField
//  }
//}

class MappingFieldProvider[A](mapping:List[(String, A)]) extends TypeFieldProvider[Mapping[A]]{
  def field[FD <: MFieldDescriptor[_]](implicit fieldDescriptor: FD): (Option[Mapping[A]]) => AbstractField[Mapping[A]] = {
    value =>
      new CustomField[Mapping[A]] {
        def getType: Class[_ <: Mapping[A]] = classOf[Mapping[A]]

        val innerField: ComboBox = new ComboBox()
        mapping.foreach {
          case (n, v) =>
            innerField.addItem(v)
            innerField.setItemCaption(v, n)
        }
        value.foreach(innerField.setValue)

        def initContent(): Component = innerField

        override def setValue(newFieldValue: Mapping[A]) = innerField.setValue(newFieldValue.asInstanceOf[Mapping[A]])

        override def getValue: Mapping[A] = innerField.getValue.asInstanceOf[A]
      }
  }
}