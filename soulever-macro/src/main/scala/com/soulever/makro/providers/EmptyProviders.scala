package com.soulever.makro.providers

/**
 * @Auther tiran 
 * @Date 7/4/14.
 */

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

trait EmptyProvider[A] {
  def empty:A
}

object EmptyProvider {
  implicit def materializeNumTypeEmptyProvider[A <% Long]:EmptyProvider[A] = macro materializeNumTypeEmptyProvider_impl[A]

  def materializeNumTypeEmptyProvider_impl[A:c.WeakTypeTag](c:Context)(evidence$1: c.Expr[A => Long]) = {
    import c.universe._
    val tag: WeakTypeTag[A] = implicitly[WeakTypeTag[A]]
    val tree: Tree = q"""
       new TypeEmptyProvider[${tag.tpe}]{
         def empty = 0
       }
       """
    tree
  }

  implicit def materializeEnumTypeEmptyProvider[A <: Enumeration]:EmptyProvider[A#Value] = macro materializeEnumTypeEmptyProvider_impl[A]

  def materializeEnumTypeEmptyProvider_impl[A: c.WeakTypeTag](c:Context) = {
    import c.universe._
    val tag: WeakTypeTag[A] = implicitly[WeakTypeTag[A]]
    val tree: Tree = q"""
       new TypeEmptyProvider[${tag}#Value]{
         def empty = ${tag.tpe.typeSymbol.name.toTermName}.values.toList.head
       }
       """
    tree
  }

  implicit def materializeEmptyMethodTypeEmptyProvider[A]:EmptyProvider[A] = macro materializeEmptyMethodTypeEmptyProvider_impl[A]

  def materializeEmptyMethodTypeEmptyProvider_impl[A :c.WeakTypeTag](c:Context) = {
    import c.universe._
    val tag: WeakTypeTag[A] = implicitly[WeakTypeTag[A]]
    val tree: Tree = q"""
       new TypeEmptyProvider[${tag.tpe}]{
         def empty = ${tag.tpe.finalResultType.typeSymbol.companion}.empty
       }
       """
    tree
  }
}
