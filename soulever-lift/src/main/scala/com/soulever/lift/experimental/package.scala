package com.soulever.lift

/**
 * @Auther tiran 
 * @Date 6/28/14.
 */
package object experimental {
  implicit class RegexContext(sc: StringContext) {
    object r {
      def apply (args : Any*) : String = {
        sc.s (args.zipWithIndex.map(a => "#" + a._2) : _*)
      }

      def unapplySeq (s : String) : Option[Seq[String]] = {
        val regexp = sc.parts.mkString.r
        regexp.unapplySeq (s)
      }
    }
  }
}
