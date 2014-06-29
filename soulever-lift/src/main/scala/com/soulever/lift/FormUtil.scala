package com.soulever.lift

import com.soulever.makro.{FieldValidation2, FieldValidation, Macros}
import language.experimental.macros
import scala.reflect.runtime.{universe => ru}
import ru._
import scala.reflect.macros.Context
import scala.xml.Elem

object FormUtil {
  def form[A <: Product, FD <: FieldDescriptor](init:A, action:A => Either[Exception, A])
                                               (implicit modleDesc:FD):Elem = macro form_macro[A, FD]

  def field[FieldType, FD <: FieldDescriptor, ClassType](init:FieldType,
                                                         i18nKey:String,
                                                         ths:ClassType)
                                                        (implicit moduleDesc:FD):FD#BaseFieldType[FieldType, ClassType] = macro field_macro[FieldType, FD, ClassType]


  def form_macro[A: c.WeakTypeTag, FD:c.WeakTypeTag](c:Context)
                                                    (init:c.Expr[A], action:c.Expr[A => Either[Exception, A]])
                                                    (modleDesc:c.Expr[FD]):c.Expr[Elem] = {
    import c.universe._

    c.Expr[Elem]( q"""
    com.soulever.makro.Macros.form[${init.actualType}, ${modleDesc.actualType}, com.vaadin.ui.FormLayout]($init, $action)
    """)
  }

  def field_macro[FieldType:c.WeakTypeTag, FD:c.WeakTypeTag, ClassType](c:Context)
                                                                       (init:c.Expr[FieldType],
                                                                        i18nKey:c.Expr[String],
                                                                        ths:c.Expr[ClassType])
                                                                       (moduleDesc:c.Expr[FD]) = {
    import c.universe._
    q"""
       com.soulever.makro.Macros.field[${init.actualType}, ${moduleDesc.actualType}, ${ths.actualType}]($init, $i18nKey, $ths)
       """
  }
}
