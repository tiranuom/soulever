package com.soulever.makro.providers

import com.soulever.makro.MFieldDescriptor

/**
 * @Auther tiran 
 * @Date 7/4/14.
 */

trait TypeFieldProvider[A, Field[_], IFD <: MFieldDescriptor[IFD]] {
  def field[FD <: MFieldDescriptor[_]](fieldDescriptor:FD)
                                      (op:A, baseField: IFD#BaseFieldType[_, _]): Field[A]

  def empty:A
}

trait KindFieldProvider[A[_], Field[_], IFD <: MFieldDescriptor[IFD]]{
  def field[B, FD <: MFieldDescriptor[_]](innerField:(B, IFD#BaseFieldType[_, _]) => Field[B], empty:B, fieldDescriptor:FD)
  (op:A[B], baseField: IFD#BaseFieldType[_, _]): Field[A[B]]

  def empty[B]:A[B]
}

