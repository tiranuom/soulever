package com.soulever.makro

trait TypeFieldProvider[A, Field[_]] {
  def field[FD <: MFieldDescriptor[_]](implicit fieldDescriptor:FD):Option[A] => Field[A]

  def empty:A
}

trait KindFieldProvider[A[_], Field[_]]{
  def field[B, FD <: MFieldDescriptor[_]](innerField:Option[B] => Field[B])
                                         (implicit fieldDescriptor:FD):Option[A[B]] => Field[A[B]]

  def empty[B]:A[B]
}
