package com.soulever.vaadin

import com.soulever.makro.Macros
import com.vaadin.ui.FormLayout
import language.experimental.macros
import scala.reflect.macros.Context

object FormUtil {
  def form[A <: Product, FD <: FieldDescriptor](init:A, action:A => Either[Exception, A])
                        (implicit modleDesc:FD):FormLayout = macro form_macro[A, FD]

  def form_macro[A: c.WeakTypeTag, FD:c.WeakTypeTag](c:Context)
                                  (init:c.Expr[A], action:c.Expr[A => Either[Exception, A]])
                                  (modleDesc:c.Expr[FD]):c.Expr[FormLayout] = {
    import c.universe._

    c.Expr[FormLayout]( q"""
    com.soulever.makro.Macros.form[${init.actualType}, ${modleDesc.actualType}, com.vaadin.ui.FormLayout]($init, $action)
    """)
  }
}
