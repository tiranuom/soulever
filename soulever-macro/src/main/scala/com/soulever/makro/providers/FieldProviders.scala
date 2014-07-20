package com.soulever.makro.providers

import com.soulever.makro.AbstractFieldDescriptor

import scala.annotation.implicitNotFound

/**
 * @Auther tiran 
 * @Date 7/4/14.
 */

@implicitNotFound("Cannot find TypeFieldProvider for ${A}")
trait TypeFieldProvider[A, Field[_], IFD <: AbstractFieldDescriptor[IFD]] {
  def field[FD <: AbstractFieldDescriptor[_]](fieldDescriptor:FD)
                                      (op:A, baseField: IFD#BaseFieldType[_, _]): Field[A]

  def empty:A
}

@implicitNotFound("Cannot find KindFieldProvider for ${A}")
trait KindFieldProvider[A[_], Field[_], IFD <: AbstractFieldDescriptor[IFD]]{
  def field[B, FD <: AbstractFieldDescriptor[_]](innerField:(B, IFD#BaseFieldType[_, _]) => Field[B], empty:B, fieldDescriptor:FD)
  (op:A[B], baseField: IFD#BaseFieldType[_, _]): Field[A[B]]

  def empty[B]:A[B]
}

