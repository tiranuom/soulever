package com.soulever.vaadin.types

import com.soulever.makro
import com.soulever.makro.MFieldDescriptor
import com.vaadin.ui.AbstractField

trait TypeFieldProvider[A, IFD <: MFieldDescriptor[IFD]] extends makro.providers.TypeFieldProvider[A, AbstractField, IFD]

trait KindFieldProvider[A[_], IFD <: MFieldDescriptor[IFD]] extends makro.providers.KindFieldProvider[A, AbstractField, IFD]