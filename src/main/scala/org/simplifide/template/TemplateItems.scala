package org.simplifide.template

import scala.collection.mutable.ListBuffer

/**
 * Created by andy.wagner on 9/22/2014.
 */
class TemplateItems {

}

object TemplateItems {

  class Simple(val name:String) extends Template {
    var item:Option[Template] = None
    def apply(template:Template) = {item = Some(template); template}

    def create = item.map(_.create).getOrElse("")

  }

  class TemplateList(val name:String) extends Template {
    val buffer = new ListBuffer[Template]()

    def apply(template:Template*)        = {buffer.appendAll(template); template}
    def apply(templates:List[Template]) = {buffer.appendAll(templates); templates}

    def convert = new Template.ListTemplate(buffer.toList)

    def create = buffer.map(_.create).mkString("")

    override def empty = (buffer.size == 0)

  }

}
