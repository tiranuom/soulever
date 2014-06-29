package com.soulever.vaadin.providers

import com.soulever.vaadin.{GeneratedField, FieldDescriptor, TypeFieldProvider}
import com.soulever.makro.MFieldDescriptor
import com.vaadin.ui.{ComboBox, Component, CustomField, AbstractField}

class EnumerationFieldProvider[A <: Enumeration](enum:A) extends TypeFieldProvider[A#Value, FieldDescriptor]{

  override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD)(op: Option[A#Value], baseField: GeneratedField[_,_]): AbstractField[A#Value] =
    new CustomField[A#Value] {
      def getType: Class[_ <: A#Value] = classOf[A#Value]

      val innerField: ComboBox = new ComboBox()
      enum.values.foreach(innerField.addItem)
      op.foreach(innerField.setValue)
      innerField.setNullSelectionAllowed(false)

      def initContent(): Component = innerField

      override def setValue(newFieldValue: A#Value) = innerField.setValue(newFieldValue)

      override def getValue: A#Value = innerField.getValue.asInstanceOf[A#Value]

      override def focus() = innerField.focus()
    }


  override def empty: A#Value = enum.values.head
}
