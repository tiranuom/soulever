package com.soulever.vaadin.providers

import com.soulever.makro.providers.EmptyProvider
import com.soulever.vaadin.FieldDescriptor
import com.soulever.vaadin.types.{FieldProvider, GeneratedField}
import com.vaadin.data.Property
import com.vaadin.data.Property.{ValueChangeEvent, ValueChangeListener}
import com.vaadin.ui.{AbstractField, CheckBox, Component, HorizontalLayout}

class OptionFieldProvider[A : FieldProvider : EmptyProvider] extends FieldProvider[Option[A]] {
  override def field[FD <: FieldDescriptor](fieldDescriptor: FD)
                                           (op: Option[A], baseField: GeneratedField[_, _]): AbstractField[Option[A]] =
    new BaseField[Option[A]] with InlineKeyProvider with InlineValidationProvider {
      def getType: Class[_ <: Option[A]] = classOf[Option[A]]

      val checkboxField = {
        val checkBox = new CheckBox("", op.isDefined)
        checkBox.addValueChangeListener(new ValueChangeListener {
          def valueChange(event: ValueChangeEvent) = {
            innerField.setEnabled(event.getProperty.asInstanceOf[Property[Boolean]].getValue)
          }
        })
        checkBox
      }

      val innerField: AbstractField[A] = implicitly[FieldProvider[A]].
        field(fieldDescriptor)(op.getOrElse(implicitly[EmptyProvider[A]].empty), baseField)

      innerField.setEnabled(op.isDefined)

      override def initContent(): Component =
        new HorizontalLayout(checkboxField, innerField)

      override def setValue(newFieldValue: Option[A]) = {
        newFieldValue.foreach(innerField.setValue)
        checkboxField.setValue(newFieldValue.isDefined)
        innerField.setEnabled(checkboxField.getValue)
      }

      override def getValue: Option[A] = {
        if (checkboxField.getValue) Some(innerField.getValue) else None
      }

      override def validate() = if (checkboxField.getValue) innerField.validate()

      override def inlineValidations: List[(String,String)] = innerField match {
        case value: com.soulever.vaadin.providers.InlineValidationProvider => value.inlineValidations
        case _ => List.empty
      }

      override def inlineKeys: List[(String,String)] = innerField match {
        case value: com.soulever.vaadin.providers.InlineKeyProvider => value.inlineKeys
        case _ => List.empty
      }
    }
}
