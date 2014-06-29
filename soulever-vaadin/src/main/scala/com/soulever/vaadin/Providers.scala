package com.soulever.vaadin

import com.soulever.makro
import com.soulever.makro.MFieldDescriptor
import com.vaadin.ui.AbstractField

trait TypeFieldProvider[A, IFD <: MFieldDescriptor[IFD]] extends makro.TypeFieldProvider[A, AbstractField, IFD]

trait KindFieldProvider[A[_], IFD <: MFieldDescriptor[IFD]] extends makro.KindFieldProvider[A, AbstractField, IFD]