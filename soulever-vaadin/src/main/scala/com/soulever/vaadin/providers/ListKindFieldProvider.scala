package com.soulever.vaadin.providers

import com.soulever.vaadin.KindFieldProvider
import com.soulever.makro.MFieldDescriptor
import com.vaadin.ui.AbstractField

class ListKindFieldProvider extends KindFieldProvider[List]{
  def field[B, FD <: MFieldDescriptor[_]](innerField: AbstractField[B])(implicit fieldDescriptor: FD): AbstractField[List[B]] = ???
}
