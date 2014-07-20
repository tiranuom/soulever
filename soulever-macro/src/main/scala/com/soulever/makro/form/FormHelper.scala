package com.soulever.makro
package form

import language.experimental.macros
import scala.reflect.macros.blackbox
/**
 * Created by tiran on 7/18/14.
 */
trait FormHelper {

  def submit[A, B](f:A => B) = new SubmitButton(f)

  def reset[A] = new ResetButton[A]()

  def action[A, B](f: () => B) = ExternalActionButton[A, B](f)

  type ButtonBlock[A] = com.soulever.makro.form.ButtonBlock[A]
}

class FormBlockMacros(val c:blackbox.Context) {
  import c.universe._
  def form_impl[A : c.WeakTypeTag, FD : c.WeakTypeTag](instance:c.Expr[A],
                                                       buttons: c.Expr[ButtonBlock[A]]*)
                                                      (moduleDesc:c.Expr[FD]) = {
    q"com.soulever.makro.Macros.form[${instance.actualType}, ${moduleDesc.actualType}]($instance, ${buttons.toList})"
  }

  def field_impl[FieldType:c.WeakTypeTag, FD:c.WeakTypeTag, ClassType](init:c.Expr[FieldType],
                                                                        i18nKey:c.Expr[String],
                                                                        ths:c.Expr[ClassType])
                                                                       (moduleDesc:c.Expr[FD]) = {
    import c.universe._
    q"""
       com.soulever.makro.Macros.field[${init.actualType}, ${moduleDesc.actualType}, ${ths.actualType}]($init, $i18nKey, $ths)
       """
  }
}

sealed trait ButtonBlock[A] {
  def button[FD1 <: AbstractFieldDescriptor[FD1], FD <: AbstractFieldDescriptor[FD1]](label:String,
                                                                        fieldsList:List[FD1#BaseFieldType[_, A]],
                                                                        createInstance:List[FD1#BaseFieldType[_, A]] => A,
                                                                        setEmpties: () => Unit)
                                                                       (implicit fd:FD):FD1#ButtonType
  def defaultI18n: String
}

case class SubmitButton[A, B](f:A => B) extends ButtonBlock[A] {

  override def button[FD1 <: AbstractFieldDescriptor[FD1], FD <: AbstractFieldDescriptor[FD1]](label: String,
                                                                                 fieldsList: List[FD1#BaseFieldType[_, A]],
                                                                                 createInstance: (List[FD1#BaseFieldType[_, A]]) => A,
                                                                                 setEmpties: () => Unit)
                                                                                (implicit fd: FD): FD1#ButtonType =
    fd.button(label, {() =>
      Option((fieldsList foldLeft true){ case (b, f) => f.valid_? && b }).
        filter(identity).
        flatMap{ _ =>
        Option(createInstance(fieldsList)).
          filter(ob => (fieldsList foldLeft true){ case (b, f) => f.valid_?(ob) && b })
      }.map(f)
    }, fieldsList)

  def defaultI18n = "Submit"
}

case class ResetButton[A]() extends ButtonBlock[A] {
  override def button[FD1 <: AbstractFieldDescriptor[FD1], FD <: AbstractFieldDescriptor[FD1]](label: String,
                                                                                 fieldsList: List[FD1#BaseFieldType[_, A]],
                                                                                 createInstance: (List[FD1#BaseFieldType[_, A]]) => A,
                                                                                 setEmpties: () => Unit)
                                                                                (implicit fd: FD): FD1#ButtonType =
    fd.button(label, setEmpties, fieldsList)

  def defaultI18n = "Reset"
}

case class ExternalActionButton[A, B](f: () => B) extends ButtonBlock[A] {

  override def button[FD1 <: AbstractFieldDescriptor[FD1], FD <: AbstractFieldDescriptor[FD1]](label: String,
                                                                                 fieldsList: List[FD1#BaseFieldType[_, A]],
                                                                                 createInstance: (List[FD1#BaseFieldType[_, A]]) => A,
                                                                                 setEmpties: () => Unit)
                                                                                (implicit fd: FD): FD1#ButtonType =
    fd.button(label, f, fieldsList)

  def defaultI18n = "Action"
}