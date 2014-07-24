package com.soulever.vaadin.types

import com.soulever.makro
import com.soulever.makro.AbstractFieldDescriptor
import com.soulever.vaadin.FieldDescriptor
import com.vaadin.ui.AbstractField

trait TypeFieldProvider[A] extends makro.providers.TypeFieldProvider[A, AbstractField, FieldDescriptor]

trait KindFieldProvider[A[_]] extends makro.providers.KindFieldProvider[A, AbstractField, FieldDescriptor]