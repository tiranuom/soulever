package com.soulever.makro.providers

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

/**
 * Created by tiran on 7/20/14.
 */
trait FieldDescriptorImplicits {
  type TypeEmptyProvider[A] = com.soulever.makro.providers.TypeEmptyProvider[A]
  type KindEmptyProvider[A[_]] = com.soulever.makro.providers.KindEmptyProvider[A]

  implicit def materializeNumTypeEmptyProvider[A <% Long]:TypeEmptyProvider[A] =
    macro EmptyProviderMacros.materializeNumTypeEmptyProvider_impl[A]

  implicit def materializeEnumTypeEmptyProvider[A <: Enumeration]:TypeEmptyProvider[A#Value] =
    macro EmptyProviderMacros.materializeEnumTypeEmptyProvider_impl[A]

  implicit def materializeEmptyMethodTypeEmptyProvider[A]:TypeEmptyProvider[A] =
    macro EmptyProviderMacros.materializeEmptyMethodTypeEmptyProvider_impl[A]

  implicit def materializeKindEmptyProvider[A[_]]: KindEmptyProvider[A] =
    macro EmptyProviderMacros.materializeKindEmptyProvider_impl[A[_]]

  implicit val stringEmptyProvider = new TypeEmptyProvider[String] {
    override def empty: String = ""
  }

}

class EmptyProviderMacros(val c:Context) {
  import c.universe._
  def materializeNumTypeEmptyProvider_impl[A:c.WeakTypeTag](evidence$1: c.Expr[A => Long]) = {
    val tag: WeakTypeTag[A] = implicitly[WeakTypeTag[A]]
    val tree: Tree = q"""
       new TypeEmptyProvider[${tag.tpe}]{
         def empty = 0
       }
       """
    tree
  }

  def materializeEnumTypeEmptyProvider_impl[A: c.WeakTypeTag] = {
    val tag: WeakTypeTag[A] = implicitly[WeakTypeTag[A]]
    val tree: Tree = q"""
       new TypeEmptyProvider[${tag}#Value]{
         def empty = ${tag.tpe.typeSymbol.name.toTermName}.values.toList.head
       }
       """
    tree
  }

  def materializeEmptyMethodTypeEmptyProvider_impl[A :c.WeakTypeTag] = {
    val tag: WeakTypeTag[A] = implicitly[WeakTypeTag[A]]
    val tree: Tree = q"""
       new TypeEmptyProvider[${tag.tpe}]{
         def empty = ${tag.tpe.finalResultType.typeSymbol.companion}.empty
       }
       """
    tree
  }

  def materializeKindEmptyProvider_impl[A:c.WeakTypeTag] = {
    val tag: WeakTypeTag[A] = implicitly[WeakTypeTag[A]]
    val tree: Tree = q"""
       new KindEmptyProvider[${tag.tpe}]{
         def empty[B] = ${tag.tpe.finalResultType.typeSymbol.companion}.empty[B]
       }
       """
    tree
  }
}