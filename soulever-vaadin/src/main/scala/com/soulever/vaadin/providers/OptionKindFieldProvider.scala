package com.soulever.vaadin.providers

import com.soulever.vaadin.KindFieldProvider
import com.soulever.makro.MFieldDescriptor
import com.vaadin.ui.{HorizontalLayout, Component, CheckBox, AbstractField}
import com.vaadin.data.Property.{ValueChangeEvent, ValueChangeListener}
import com.vaadin.data.Property

class OptionKindFieldProvider extends KindFieldProvider[Option] {

  override def field[B, FD <: MFieldDescriptor[_]](inf: (Option[B]) => AbstractField[B])
                                                  (fieldDescriptor: FD)
                                                  (op: Option[Option[B]]): AbstractField[Option[B]] =
    new BaseField[Option[B]] {
      def getType: Class[_ <: Option[B]] = classOf[Option[B]]

      val checkboxField = {
        val checkBox = new CheckBox("", op.flatMap(identity).isDefined)
        checkBox.addValueChangeListener(new ValueChangeListener {
          def valueChange(event: ValueChangeEvent) = {
            innerField.setEnabled(event.getProperty.asInstanceOf[Property[Boolean]].getValue)
          }
        })
        checkBox
      }

      val innerField: AbstractField[B] = inf(op.flatMap(identity))
      innerField.setEnabled(op.flatMap(identity).isDefined)

      override def initContent(): Component =
        new HorizontalLayout(checkboxField, innerField)

      override def setValue(newFieldValue: Option[B]) = {
        newFieldValue.foreach(innerField.setValue)
        checkboxField.setValue(newFieldValue.isDefined)
        innerField.setEnabled(checkboxField.getValue)
      }

      override def getValue: Option[B] = {
        if (checkboxField.getValue) Some(innerField.getValue) else None
      }

      override def validate() = if (checkboxField.getValue) innerField.validate()
    }


  override def empty[B]: Option[B] = None
}
