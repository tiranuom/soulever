package com.soulever.vaadin.providers

import com.soulever.vaadin.{TypeFieldProvider, KindFieldProvider}
import com.soulever.makro.MFieldDescriptor
import com.vaadin.ui._
import com.soulever.makro.types.Mapping

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
        innerField.setNullSelectionAllowed(false)
        value.foreach(innerField.setValue)

        def initContent(): Component = innerField

        override def setValue(newFieldValue: Mapping[A]) = innerField.setValue(newFieldValue.asInstanceOf[Mapping[A]])

        override def getValue: Mapping[A] = innerField.getValue.asInstanceOf[A]

        override def focus() = innerField.focus()
      }
  }

  override def empty: Mapping[A] = new Mapping[A](mapping.head._2) //Might produce an exception
}