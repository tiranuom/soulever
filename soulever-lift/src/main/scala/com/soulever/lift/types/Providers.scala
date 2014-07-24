package com.soulever.lift.types

import com.soulever.lift.FieldDescriptor
import com.soulever.makro
import com.soulever.makro.AbstractFieldDescriptor

/**
 * @Auther tiran 
 * @Date 6/28/14.
 */

trait TypeFieldProvider[A] extends makro.providers.TypeFieldProvider[A, InnerField, FieldDescriptor]

trait KindFieldProvider[A[_]] extends makro.providers.KindFieldProvider[A, InnerField, FieldDescriptor]
