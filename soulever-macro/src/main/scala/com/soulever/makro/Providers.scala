package com.soulever.makro

trait TypeFieldProvider[A, Field[_]] {
  def field[FD <: MFieldDescriptor[_]](implicit fieldDescriptor:FD):Field[A]
}

trait KindFieldProvider[A[_], Field[_]]{
  def field[B, FD <: MFieldDescriptor[_]](innerField:Field[B])(implicit fieldDescriptor:FD):Field[A[B]]
}
