package com.soulever.makro.experimantal

/**
 * Created by tiran on 7/11/14.
 */

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macro {
  def tplExpand(ctx:Context)(tree:ctx.Tree):Unit = macro tplExpand_impl

  def tplExpand_impl(c:Context)(ctx:c.Tree)(tree:c.Tree) = {
    import c.universe._
    println(tree)
    q""
  }
}
