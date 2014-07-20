package com.soulever.vaadin.providers

import com.soulever.vaadin.FieldDescriptor
import com.soulever.makro.AbstractFieldDescriptor
import com.soulever.vaadin.types.KindFieldProvider
import com.vaadin.ui.{HorizontalLayout, Component, CheckBox, AbstractField}
import com.vaadin.data.Property.{ValueChangeEvent, ValueChangeListener}
import com.vaadin.data.Property

class OptionKindFieldProvider extends KindFieldProvider[Option, FieldDescriptor] {

  override def field[B, FD <: AbstractFieldDescriptor[_]](inf: (B, FieldDescriptor#BaseFieldType[_,_]) => AbstractField[B], empty:B, fieldDescriptor: FD)
                                                  (op: Option[B], baseField: FieldDescriptor#BaseFieldType[_, _]): AbstractField[Option[B]] =
    new BaseField[Option[B]] with InlineKeyProvider with InlineValidationProvider {
      def getType: Class[_ <: Option[B]] = classOf[Option[B]]

      val checkboxField = {
        val checkBox = new CheckBox("", op.isDefined)
        checkBox.addValueChangeListener(new ValueChangeListener {
          def valueChange(event: ValueChangeEvent) = {
            innerField.setEnabled(event.getProperty.asInstanceOf[Property[Boolean]].getValue)
          }
        })
        checkBox
      }

      val innerField: AbstractField[B] = inf(op.getOrElse(empty), baseField)
      innerField.setEnabled(op.isDefined)

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
