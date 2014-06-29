package com.soulever.makro

trait TypeFieldProvider[A, Field[_], IFD <: MFieldDescriptor[IFD]] {
  def field[FD <: MFieldDescriptor[_]](fieldDescriptor:FD)
                                      (op:Option[A], baseField: IFD#BaseFieldType[_, _]): Field[A]

  def empty:A
}

trait KindFieldProvider[A[_], Field[_], IFD <: MFieldDescriptor[IFD]]{
  def field[B, FD <: MFieldDescriptor[_]](innerField:(Option[B], IFD#BaseFieldType[_, _]) => Field[B], empty:B, fieldDescriptor:FD)
                                         (op:Option[A[B]], baseField: IFD#BaseFieldType[_, _]): Field[A[B]]

  def empty[B]:A[B]
}
