package com.soulever.vaadin.providers

import com.soulever.vaadin.KindFieldProvider
import com.soulever.makro.MFieldDescriptor
import com.vaadin.ui._

class ListKindFieldProvider extends KindFieldProvider[List]{
  def field[B, FD <: MFieldDescriptor[_]](innerField: (Option[B]) => AbstractField[B])(implicit fieldDescriptor: FD): (Option[List[B]]) => AbstractField[List[B]] = {
    value =>
      new CustomField[List[B]] {
        def getType: Class[_ <: List[B]] = classOf[List[B]]

        def createFieldRow(o:Option[B]) = {
          val layout = new HorizontalLayout()
          val removeButton: Button = new Button("-", new Button.ClickListener {
            def buttonClick(event: Button.ClickEvent) {
              fieldsList = fieldsList.filter(_.hashCode() != layout.hashCode())
              vLayout.removeAllComponents()
              vLayout.addComponents(fieldsList ::: List(addButton) : _*)
            }
          })
          layout.addComponents(innerField(o), removeButton)
          layout
        }

        val vLayout = new VerticalLayout()

        val addButton:Button = new Button("+", new Button.ClickListener {
          def buttonClick(event: Button.ClickEvent) {
            fieldsList = fieldsList ::: List(createFieldRow(None))
            vLayout.removeAllComponents()
            vLayout.addComponents(fieldsList ::: List(addButton) : _*)
          }
        })

        var fieldsList:List[HorizontalLayout] = value.getOrElse(List.empty).
          map(b => createFieldRow(Option(b)))

        vLayout.addComponents(fieldsList ::: List(addButton) : _*)
        def initContent(): Component = vLayout
      }
  }
}
