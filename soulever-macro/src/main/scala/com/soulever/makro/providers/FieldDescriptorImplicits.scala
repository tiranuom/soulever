package com.soulever.makro.providers

import com.soulever.makro.AbstractFieldDescriptor

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

/**
 * Created by tiran on 7/20/14.
 */
trait FieldDescriptorImplicits extends LowPriorityFieldDescriptorImplicits{

  type EmptyProvider[A] = com.soulever.makro.providers.EmptyProvider[A]

  type FieldDescriptor <: AbstractFieldDescriptor[FieldDescriptor]

  implicit def materializeNumTypeEmptyProvider[A <% Long]:EmptyProvider[A] =
    macro EmptyProviderMacros.materializeNumEmptyProvider_impl[A]

  implicit def materializeEnumTypeEmptyProvider[A <: Enumeration]:EmptyProvider[A#Value] =
    macro EmptyProviderMacros.materializeEnumEmptyProvider_impl[A]

  implicit val stringEmptyProvider = new EmptyProvider[String] {
    override def empty: String = ""
  }
}

trait LowPriorityFieldDescriptorImplicits {
  implicit def materializeEmptyMethodTypeEmptyProvider[A]:EmptyProvider[A] =
    macro EmptyProviderMacros.materializeEmptyMethodEmptyProvider_impl[A]
}

class EmptyProviderMacros(val c:Context) {
  import c.universe._
  def materializeNumEmptyProvider_impl[A:c.WeakTypeTag](evidence$1: c.Expr[A => Long]) = {
    val tag: WeakTypeTag[A] = implicitly[WeakTypeTag[A]]
    val tree: Tree = q"""
       new EmptyProvider[${tag.tpe}]{
         def empty = 0
       }
       """
    tree
  }

  def materializeEnumEmptyProvider_impl[A: c.WeakTypeTag] = {
    val tag: WeakTypeTag[A] = implicitly[WeakTypeTag[A]]
    val tree: Tree = q"""
       new EmptyProvider[${tag}#Value]{
         def empty = ${tag.tpe.typeSymbol.name.toTermName}.values.toList.head
       }
       """
    tree
  }

  def materializeEmptyMethodEmptyProvider_impl[A :c.WeakTypeTag] = {
    val tag: WeakTypeTag[A] = implicitly[WeakTypeTag[A]]
    val tree: Tree = q"""
       new EmptyProvider[${tag.tpe}]{
         def empty = ${tag.tpe.finalResultType.typeSymbol.companion}.empty
       }
       """
    tree
  }
}