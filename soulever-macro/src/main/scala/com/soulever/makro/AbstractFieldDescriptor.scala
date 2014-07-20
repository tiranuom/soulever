package com.soulever.makro

import com.soulever.makro.i18n.I18nKeyCollector
import com.soulever.makro.providers.TypeFieldProvider
import Soulever._

trait AbstractFieldDescriptor[FieldDescriptor <: AbstractFieldDescriptor[FieldDescriptor]] {
  type LayoutType
  type FieldType[_]
  type ButtonType
  type BaseFieldType[A, Obj] <: BaseField[A, Obj]
  type RequestType

  def fieldComponent[A : Manifest, Obj](init:A,
                               caption:String,
                               innerField:(A, FieldDescriptor#BaseFieldType[A, Obj]) => FieldDescriptor#FieldType[A],
                               validators:List[A => Either[String, A]] = List.empty,
                               secondaryValidators:List[(A, Obj) => Either[String, A]] = List.empty,
                               css:String = ""):FieldDescriptor#BaseFieldType[A, Obj]

  def button(label:String, action:() => Any, fields:List[FieldDescriptor#BaseFieldType[_, _]]):FieldDescriptor#ButtonType

  def i18n(msg:String, defaultValue:Option[String] = None):String = i18nKeyCollector.i18n(msg, defaultValue)

  def formComponent(fields:List[FieldDescriptor#BaseFieldType[_, _]],
                    buttons:List[FieldDescriptor#ButtonType]):FieldDescriptor#LayoutType

  def mappingFieldProvider[A](mapping:List[(String, A)]):TypeFieldProvider[Mapping[A], FieldDescriptor#FieldType, FieldDescriptor]

  def enumFieldProvider[A <: Enumeration](enum:A):TypeFieldProvider[A#Value, FieldDescriptor#FieldType, FieldDescriptor]

  val i18nKeyCollector:I18nKeyCollector
}

trait BaseField[A, Obj] {

  def valid_? :Boolean

  def valid_?(obj:Obj):Boolean

  def value_=(value:A)

  def value:A

  def innerValidations:List[(String, String)]

  def innerI18nKeys:List[(String,String)]
}
