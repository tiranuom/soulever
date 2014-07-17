package com.soulever.metamacro

import language.experimental.macros
import scala.reflect.macros.blackbox
import scala.reflect.macros.whitebox

/**
 * @Auther tiran 
 * @Date 7/4/14.
 */
object Meta {

  def tplExpand(ctx:blackbox.Context)(tree:ctx.Type):Unit = macro tplExpand_impl

  def tplExpand_impl(c:whitebox.Context)(ctx:c.Tree)(tree:c.Tree) = {
    import c.universe._
    println(tree)
    q""
  }

}
