package com.soulever.makro

import com.soulever.makro.types.Mapping

trait MFieldDescriptor[LayoutType] {
  type FieldType[_]
  type ButtonType
  type BaseFieldType[A] <: BaseField[A, _]

  def field[A : Manifest, Obj](init:A,
                               caption:String,
                               innerField:Option[A] => FieldType[A],
                               validators:List[A => Either[String, A]] = List.empty,
                               secondaryValidators:List[(A, Obj) => Either[String, A]] = List.empty,
                               css:String = "",
                               prefix:String = "",
                               postfix:String = ""):BaseFieldType[A]

  def submitButton(label:String, clickAction:() => Unit):ButtonType

  def i18n(msg:String):String = msg

  def form(fields:List[FieldType[_]], buttons:List[ButtonType]):LayoutType

  def mappingFieldProvider[A](mapping:List[(String, A)]):TypeFieldProvider[Mapping[A], FieldType]

  def enumFieldProvider[A <: Enumeration](enum:A):TypeFieldProvider[A#Value, FieldType]
}

trait BaseField[A, Obj] {

  def isValid:Boolean

  def isValid(obj:Obj):Boolean

  def getValue:A

  def innerValidations:List[String]
}