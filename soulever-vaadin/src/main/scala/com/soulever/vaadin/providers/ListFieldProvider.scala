package com.soulever.vaadin.providers

import com.soulever.makro.providers.EmptyProvider
import com.soulever.vaadin.FieldDescriptor
import com.soulever.vaadin.types.FieldProvider
import com.vaadin.ui._

class ListFieldProvider[A : FieldProvider : EmptyProvider] extends FieldProvider[List[A]] {
  override def field[FD <: FieldDescriptor](fieldDescriptor: FD)
                                           (op: List[A], baseField: FD#BaseFieldType[_, _]): FieldDescriptor#FieldType[List[A]] =
    new CustomField[List[A]] with InlineValidationProvider with InlineKeyProvider {
      def getType: Class[_ <: List[A]] = classOf[List[A]]

      val innerField = implicitly[FieldProvider[A]].field(fieldDescriptor)(_,_)
      val empty = implicitly[EmptyProvider[A]].empty

      def createFieldRow(o:A) = {
        val layout = new HorizontalLayout()
        val removeButton: Button = new Button("-", new Button.ClickListener {
          def buttonClick(event: Button.ClickEvent) {
            fieldsList = fieldsList.filter(_._1.hashCode() != layout.hashCode())
            vLayout.removeAllComponents()
            vLayout.addComponents(fieldsList.map(_._1) ::: List(addButton) : _*)
          }
        })
        val field: AbstractField[A] = innerField(o, baseField)
        layout.addComponents(field, removeButton)
        layout -> field
      }

      val vLayout = new VerticalLayout()

      val addButton:Button = new Button("+", new Button.ClickListener {
        def buttonClick(event: Button.ClickEvent) {
          fieldsList = fieldsList ::: List(createFieldRow(empty))
          vLayout.removeAllComponents()
          vLayout.addComponents(fieldsList.map(_._1) ::: List(addButton) : _*)
        }
      })

      var fieldsList:List[(HorizontalLayout, AbstractField[A])] = op.
        map(createFieldRow)

      vLayout.addComponents(fieldsList.map(_._1) ::: List(addButton) : _*)

      def initContent(): Component = vLayout

      override def validate() = {
        fieldsList.foreach(_._2.validate())
      }

      override def getValue: List[A] = fieldsList.map(_._2.getValue)

      override def setValue(newFieldValue: List[A]) = {
        fieldsList = op.map(createFieldRow)
        vLayout.removeAllComponents()
        vLayout.addComponents(fieldsList.map(_._1) ::: List(addButton) : _*)
      }

      override def inlineValidations: List[(String,String)] = innerField(empty, baseField) match {
        case value: com.soulever.vaadin.providers.InlineValidationProvider => value.inlineValidations
        case _ => List.empty
      }

      override def inlineKeys: List[(String,String)] = innerField(empty, baseField) match {
        case value: com.soulever.vaadin.providers.InlineKeyProvider => value.inlineKeys
        case _ => List.empty
      }
    }
}