package com.soulever.makro

/**
 * Created by tiran on 7/16/14.
 */
package object annotations {

  trait =/>[-A, +B] extends PartialFunction[A, B] {
    def &[A1 <: A, B1 >: B] (f:PartialFunction[A1, B1]) = this.orElse(f)
  }

  implicit def partialFunctionToArrow[A, B](pf:PartialFunction[A, B]): (A =/> B) = new =/>[A, B]{
    override def isDefinedAt(x: A): Boolean = pf.isDefinedAt(x)

    override def apply(v1: A): B = pf.apply(v1)
  }
}
