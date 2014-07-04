package com.soulever.vaadin.providers

import com.soulever.vaadin.FieldDescriptor
import com.soulever.makro.MFieldDescriptor
import com.soulever.vaadin.types.KindFieldProvider
import com.vaadin.ui._

class ListKindFieldProvider extends KindFieldProvider[List, FieldDescriptor]{

  override def field[B, FD <: MFieldDescriptor[_]](innerField: (B, FieldDescriptor#BaseFieldType[_, _]) => AbstractField[B], empty: B, fieldDescriptor: FD)
                                                  (op: List[B], baseField: FieldDescriptor#BaseFieldType[_, _]): AbstractField[List[B]] =
    new CustomField[List[B]] with InlineValidationProvider with InlineKeyProvider {
      def getType: Class[_ <: List[B]] = classOf[List[B]]

      def createFieldRow(o:B) = {
        val layout = new HorizontalLayout()
        val removeButton: Button = new Button("-", new Button.ClickListener {
          def buttonClick(event: Button.ClickEvent) {
            fieldsList = fieldsList.filter(_._1.hashCode() != layout.hashCode())
            vLayout.removeAllComponents()
            vLayout.addComponents(fieldsList.map(_._1) ::: List(addButton) : _*)
          }
        })
        val field: AbstractField[B] = innerField(o, baseField)
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

      var fieldsList:List[(HorizontalLayout, AbstractField[B])] = op.
        map(createFieldRow)

      vLayout.addComponents(fieldsList.map(_._1) ::: List(addButton) : _*)

      def initContent(): Component = vLayout

      override def validate() = {
        fieldsList.foreach(_._2.validate())
      }

      override def getValue: List[B] = fieldsList.map(_._2.getValue)

      override def setValue(newFieldValue: List[B]) = {
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

  override def empty[B]: List[B] = List.empty
}
