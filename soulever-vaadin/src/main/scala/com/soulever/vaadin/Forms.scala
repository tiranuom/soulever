package com.soulever.vaadin

import com.soulever.makro.form.FormHelper
import com.vaadin.ui.FormLayout

import scala.language.experimental.macros

object Forms extends FormHelper {
  def form[A, FD <: FieldDescriptor](instance:A, buttons: ButtonBlock[A]*)
                                               (implicit moduleDesc:FD):FormLayout =
    macro com.soulever.makro.form.FormBlockMacros.form_impl[A, FD]

  def field[FieldType, FD <: FieldDescriptor, ClassType](init:FieldType,
                                                         i18nKey:String,
                                                         ths:ClassType)
                                                        (implicit moduleDesc:FD):FD#BaseFieldType[FieldType, ClassType] =
    macro com.soulever.makro.form.FormBlockMacros.field_impl[FieldType, FD, ClassType]
}
