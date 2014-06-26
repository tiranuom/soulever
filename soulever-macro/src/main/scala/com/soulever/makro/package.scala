package com.soulever

/**
 * @Auther tiran 
 * @Date 6/25/14.
 */
package object makro {
  implicit class Unzip4[A,B,C,D](val xs: List[(A,B,C,D)]) extends AnyVal {
    def unzip4: (List[A], List[B], List[C], List[D]) = xs.foldRight[(List[A], List[B], List[C], List[D])]((Nil,Nil,Nil,Nil)) { (x, res) =>
      val (a,b,c,d) = x
      (a :: res._1, b :: res._2, c :: res._3, d :: res._4)
    }
  }

  implicit class StringHelper(s:String) {
    def dotNotation = s.
      replaceAll("(?<=[A-Z])(?=[A-Z][a-z])|(?<=[^A-Z])(?=[A-Z])|(?<=[A-Za-z])(?=[^A-Za-z])", ".").
      toLowerCase.
      trim
  }
}
