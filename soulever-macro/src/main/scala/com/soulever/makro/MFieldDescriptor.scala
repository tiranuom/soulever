package com.soulever.makro

trait MFieldDescriptor[LayoutType] {
  type FieldType[_]
  type ButtonType
  type BaseFieldType[A] <: BaseField[A]

  def field[A : Manifest](init:A,
                          caption:String,
                          innerField:Option[A] => FieldType[A],
                          validators:List[A => Either[String, A]] = List.empty,
                          prefix:String = "",
                          postfix:String = "",
                          i18n:String => String = identity):BaseFieldType[A]

  def submitButton(label:String, clickAction:() => Unit):ButtonType

  def i18n(msg:String):String = msg

  def form(fields:List[FieldType[_]], buttons:List[ButtonType]):LayoutType
}

trait BaseField[A] {

  def isValid:Boolean

  def getValue:A
}