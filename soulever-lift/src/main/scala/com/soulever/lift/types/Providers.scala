package com.soulever.lift.types

import com.soulever.makro
import com.soulever.makro.MFieldDescriptor

/**
 * @Auther tiran 
 * @Date 6/28/14.
 */

trait TypeFieldProvider[A, IFD <: MFieldDescriptor[IFD]] extends makro.providers.TypeFieldProvider[A, InnerField, IFD]

trait KindFieldProvider[A[_], IFD <: MFieldDescriptor[IFD]] extends makro.providers.KindFieldProvider[A, InnerField, IFD]
