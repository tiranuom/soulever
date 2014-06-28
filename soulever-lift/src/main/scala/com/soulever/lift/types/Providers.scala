package com.soulever.lift.types

import com.soulever.makro

/**
 * @Auther tiran 
 * @Date 6/28/14.
 */

trait TypeFieldProvider[A] extends makro.TypeFieldProvider[A, InnerField]

trait KindFieldProvider[A[_]] extends makro.KindFieldProvider[A, InnerField]
