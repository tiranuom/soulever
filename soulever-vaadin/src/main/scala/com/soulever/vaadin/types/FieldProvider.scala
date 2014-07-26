package com.soulever.vaadin.types

import com.soulever.makro
import com.soulever.makro.AbstractFieldDescriptor
import com.soulever.vaadin.FieldDescriptor
import com.vaadin.ui.AbstractField

trait FieldProvider[A] extends makro.providers.FieldProvider[A, FieldDescriptor]