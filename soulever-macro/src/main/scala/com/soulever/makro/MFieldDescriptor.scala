package com.soulever.makro

import com.soulever.makro.providers.TypeFieldProvider
import com.soulever.makro.types.Mapping

trait MFieldDescriptor[SelfType <: MFieldDescriptor[SelfType]] {
  type LayoutType
  type FieldType[_]
  type ButtonType
  type BaseFieldType[A, Obj] <: BaseField[A, Obj]

  def field[A : Manifest, Obj](init:A,
                               caption:String,
                               innerField:(A, SelfType#BaseFieldType[A, Obj]) => SelfType#FieldType[A],
                               validators:List[A => Either[String, A]] = List.empty,
                               secondaryValidators:List[(A, Obj) => Either[String, A]] = List.empty,
                               css:String = ""):SelfType#BaseFieldType[A, Obj]

  def button(label:String, clickAction:() => Unit, fieldsList:List[SelfType#BaseFieldType[_, _]]):SelfType#ButtonType

  def i18n(msg:String):String = i18nKeyCollector.i18n(msg)

  def form(fields:List[SelfType#BaseFieldType[_, _]], buttons:List[SelfType#ButtonType]):LayoutType

  def mappingFieldProvider[A](mapping:List[(String, A)]):TypeFieldProvider[Mapping[A], SelfType#FieldType, SelfType]

  def enumFieldProvider[A <: Enumeration](enum:A):TypeFieldProvider[A#Value, SelfType#FieldType, SelfType]

  val i18nKeyCollector:I18nKeyCollector
}

trait BaseField[A, Obj] {

  def isValid:Boolean

  def isValid(obj:Obj):Boolean

  def setValue(value:A)

  def getValue:A

  def innerValidations:List[(String, String)]

  def innerI18nKeys:List[(String,String)]
}