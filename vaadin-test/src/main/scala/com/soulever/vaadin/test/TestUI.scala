package com.soulever.vaadin.test

import com.vaadin.ui.UI
import com.vaadin.server.VaadinRequest
import com.vaadin.navigator.Navigator
import com.vaadin.annotations.Theme

@Theme("mytheme")
class TestUI extends UI{
  def init(request: VaadinRequest) = {
    val navigator: Navigator = new Navigator(this, this)
    navigator.addProvider(new PersonViewProvider(this))
    this.setNavigator(navigator)
  }
}
