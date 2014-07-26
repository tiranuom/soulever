package com.soulever.lift.types

import com.soulever.lift.FieldDescriptor
import com.soulever.makro
import com.soulever.makro.AbstractFieldDescriptor

/**
 * @Auther tiran 
 * @Date 6/28/14.
 */

trait FieldProvider[A] extends makro.providers.FieldProvider[A, FieldDescriptor]
