package com.soulever.makro.providers

import com.soulever.makro.AbstractFieldDescriptor

import scala.annotation.implicitNotFound

/**
 * @Auther tiran 
 * @Date 7/4/14.
 */

@implicitNotFound("Cannot find TypeFieldProvider for ${A}")
trait FieldProvider[A, IFD <: AbstractFieldDescriptor[IFD]] {
  def field[FD <: IFD](fieldDescriptor:FD)
                      (op:A, baseField: FD#BaseFieldType[_, _]): IFD#FieldType[A]
}

