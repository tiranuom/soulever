package com.soulever.makro.providers

/**
 * @Auther tiran 
 * @Date 7/4/14.
 */

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object TypeEmptyProvider {
  implicit def materializeNumTypeEmptyProvider[A <% Long]:TypeEmptyProvider[A] = macro materializeNumTypeEmptyProvider_impl[A]

  def materializeNumTypeEmptyProvider_impl[A:c.WeakTypeTag](c:Context)(evidence$1: c.Expr[A => Long]) = {
    import c.universe._
    val tag: WeakTypeTag[A] = implicitly[WeakTypeTag[A]]
    val tree: Tree = q"""
       new TypeEmptyProvider[${tag.tpe}]{
         def empty = 0
       }
       """
    //    println(tree)
    tree
  }

  implicit def materializeEnumTypeEmptyProvider[A <: Enumeration]:TypeEmptyProvider[A#Value] = macro materializeEnumTypeEmptyProvider_impl[A]

  def materializeEnumTypeEmptyProvider_impl[A: c.WeakTypeTag](c:Context) = {
    import c.universe._
    val tag: WeakTypeTag[A] = implicitly[WeakTypeTag[A]]
    val tree: Tree = q"""
       new TypeEmptyProvider[${tag}#Value]{
         def empty = ${tag.tpe.typeSymbol.name.toTermName}.values.toList.head
       }
       """

    println(tree)
    tree
  }

  implicit def materializeEmptyMethodTypeEmptyProvider[A]:TypeEmptyProvider[A] = macro materializeEmptyMethodTypeEmptyProvider_impl[A]

  def materializeEmptyMethodTypeEmptyProvider_impl[A :c.WeakTypeTag](c:Context) = {
    import c.universe._
    val tag: WeakTypeTag[A] = implicitly[WeakTypeTag[A]]
    val tree: Tree = q"""
       new TypeEmptyProvider[${tag.tpe}]{
         def empty = ${tag.tpe.finalResultType.typeSymbol.companion}.empty
       }
       """

    //    println(tree)
    tree
  }
}

trait TypeEmptyProvider[A] {
  def empty:A
}

trait KindEmptyProvider[A[_]] {
  def empty[B]:A[B]
}

object KindEmptyProvider {
  implicit def materializeKindEmptyProvider[A[_]]: KindEmptyProvider[A] = macro materializeKindEmptyProvider_impl[A[_]]

  def materializeKindEmptyProvider_impl[A:c.WeakTypeTag](c:Context) = {
    import c.universe._
    val tag: WeakTypeTag[A] = implicitly[WeakTypeTag[A]]
    println(tag.tpe)
    val tree: Tree = q"""
       new KindEmptyProvider[${tag.tpe}]{
         def empty[B] = ${tag.tpe.finalResultType.typeSymbol.companion}.empty[B]
       }
       """
    //    println(tree)
    tree
  }
}

