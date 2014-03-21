package com.soulever.makro

trait TypeFieldProvider[A, Field[_]] {
  def field[FD <: MFieldDescriptor[_]](fieldDescriptor:FD)(op:Option[A]): Field[A]

  def empty:A
}

trait KindFieldProvider[A[_], Field[_]]{
  def field[B, FD <: MFieldDescriptor[_]](innerField:Option[B] => Field[B], empty:B)
                                         (fieldDescriptor:FD)(op:Option[A[B]]): Field[A[B]]

  def empty[B]:A[B]
}
