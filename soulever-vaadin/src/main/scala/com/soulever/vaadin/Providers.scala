package com.soulever.vaadin

import com.soulever.makro
import com.vaadin.ui.AbstractField

trait TypeFieldProvider[A] extends makro.TypeFieldProvider[A, AbstractField]

trait KindFieldProvider[A[_]] extends makro.KindFieldProvider[A, AbstractField]