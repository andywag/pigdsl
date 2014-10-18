package org.simplifide.template

/**
 * Created by andy.wagner on 9/26/2014.
 */
trait TemplateConstructor {

  def creator(input:Template):Template
  def ->(template:Template) = new TemplateConstructor.Constructor(this,template)
  def create(input:Template) = creator(input).create

}

object TemplateConstructor {

  class Constructor(val instance:TemplateConstructor, val template:Template) extends Template {
    def ->(template:Template) = new TemplateConstructor.Constructor(this.instance,this.template ~ template)
    def create = instance.create(template)
  }

}
