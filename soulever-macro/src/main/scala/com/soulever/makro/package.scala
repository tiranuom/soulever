package com.soulever

/**
 * @Auther tiran 
 * @Date 6/25/14.
 */
package object makro {
  implicit class StringHelper(s:String) {
    def dotNotation = s.
      replaceAll("(?<=[A-Z])(?=[A-Z][a-z])|(?<=[^A-Z])(?=[A-Z])|(?<=[A-Za-z])(?=[^A-Za-z])", ".").
      toLowerCase.
      trim

    def naturalNotation = s.split("\\.").map(_.capitalize).mkString(" ").replaceAll("[\\[\\]\\{\\}]", "")
  }
}
