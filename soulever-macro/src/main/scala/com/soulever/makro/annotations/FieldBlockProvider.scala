package com.soulever.makro
package annotations

import scala.reflect.macros.blackbox.Context

/**
 * @Auther tiran 
 * @Date 7/6/14.
 */
trait FieldBlockProvider {
  type AnnotationType
  def validate[A:c.WeakTypeTag](c:Context)(field:c.universe.Symbol)(v:c.universe.Annotation):Unit
  def generateCodeBlock[A:c.WeakTypeTag](c:Context)(field:c.universe.Symbol, i18nKey:c.universe.Tree)(v:c.universe.Annotation):(c.universe.Tree, c.universe.Tree)
}
