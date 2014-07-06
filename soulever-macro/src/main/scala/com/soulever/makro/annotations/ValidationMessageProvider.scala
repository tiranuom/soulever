package com.soulever.makro
package annotations

/**
 * @Auther tiran 
 * @Date 7/6/14.
 */
trait ValidationMessageProvider {
  def message:String
  def defaultErrorMessage:String
}
