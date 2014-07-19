package com.soulever.lift

import com.soulever.makro.form.FormHelper

import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.reflect.runtime.{universe => ru}
import scala.xml.Elem

object Forms extends FormHelper {
  def form[A <: Product, FD <: FieldDescriptor](instance:A, buttons: ButtonBlock[A]*)
                                               (implicit moduleDesc:FD):Elem =
    macro com.soulever.makro.form.FormBlockMacros.form_impl[A, FD]

  def field[FieldType, FD <: FieldDescriptor, ClassType](init:FieldType,
                                                         i18nKey:String,
                                                         ths:ClassType)
                                                        (implicit moduleDesc:FD):FD#BaseFieldType[FieldType, ClassType] =
    macro com.soulever.makro.form.FormBlockMacros.field_impl[FieldType, FD, ClassType]
}
