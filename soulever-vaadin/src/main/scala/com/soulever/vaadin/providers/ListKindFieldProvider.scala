package com.soulever.vaadin.providers

import com.soulever.vaadin.KindFieldProvider
import com.soulever.makro.MFieldDescriptor
import com.vaadin.ui._

class ListKindFieldProvider extends KindFieldProvider[List]{

  override def field[B, FD <: MFieldDescriptor[_]](innerField: (Option[B]) => AbstractField[B], empty:B)
                                                  (fieldDescriptor: FD)
                                                  (op: Option[List[B]]): AbstractField[List[B]] =
    new CustomField[List[B]] {
      def getType: Class[_ <: List[B]] = classOf[List[B]]

      def createFieldRow(o:Option[B]) = {
        val layout = new HorizontalLayout()
        val removeButton: Button = new Button("-", new Button.ClickListener {
          def buttonClick(event: Button.ClickEvent) {
            fieldsList = fieldsList.filter(_._1.hashCode() != layout.hashCode())
            vLayout.removeAllComponents()
            vLayout.addComponents(fieldsList.map(_._1) ::: List(addButton) : _*)
          }
        })
        val field: AbstractField[B] = innerField(o)
        layout.addComponents(field, removeButton)
        layout -> field
      }

      val vLayout = new VerticalLayout()

      val addButton:Button = new Button("+", new Button.ClickListener {
        def buttonClick(event: Button.ClickEvent) {
          fieldsList = fieldsList ::: List(createFieldRow(Option(empty)))
          vLayout.removeAllComponents()
          vLayout.addComponents(fieldsList.map(_._1) ::: List(addButton) : _*)
        }
      })

      var fieldsList:List[(HorizontalLayout, AbstractField[B])] = op.getOrElse(List.empty).
        map(b => createFieldRow(Option(b)))

      vLayout.addComponents(fieldsList.map(_._1) ::: List(addButton) : _*)

      def initContent(): Component = vLayout

      override def validate() = {
        fieldsList.foreach(_._2.validate())
      }

      override def getValue: List[B] = fieldsList.map(_._2.getValue)

      override def setValue(newFieldValue: List[B]) = {
        fieldsList = op.getOrElse(List.empty).map(b => createFieldRow(Option(b)))
        vLayout.removeAllComponents()
        vLayout.addComponents(fieldsList.map(_._1) ::: List(addButton) : _*)
      }
    }

  override def empty[B]: List[B] = List.empty
}
