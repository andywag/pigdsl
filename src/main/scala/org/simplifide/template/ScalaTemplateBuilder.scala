package org.simplifide.template

import org.simplifide.template.Template._
import ScalaTemplate._

/**
 * Created by Andy on 9/18/2014.
 */

trait ScalaTemplateBuilder extends TemplateBuilder {

}

object ScalaTemplateBuilder {

  trait Extendible extends TemplateBuilder {
    val extend = new TemplateItems.TemplateList("Extends")
    lazy val extendDec = opt(" extends ", extend.buffer.size > 0) ~ sep(extend.convert," with ")
  }

  trait Container extends TemplateBuilder {
    val items   = new TemplateItems.TemplateList("Items")
  }


  trait ScalaFile extends TemplateBuilder with Container{
    val name = "ScalaFile"

    val pack:Template
    val imports = new TemplateItems.TemplateList("Imports")
    def $import(template:Template) = imports("import " ~ template)

    lazy val creator    =  "package " ~ pack ~ "\n\n" ~ sep(imports.convert,"\n") ~ "\n\n" ~ sep(items,"\n") ~ "\n"
  }


  trait ScalaObject extends TemplateBuilder with Container {
    lazy val creator = "object " ~ name ~ " {\n\n" ~ indent(sep(items.convert,"\n")) ~ "\n}\n\n"
  }

  trait ScalaTrait extends TemplateBuilder with Extendible with Container {
    lazy val creator = "trait " ~ name ~ extendDec ~ " {\n\n " ~ indent(sep(items.convert,"\n")) ~ "\n}\n\n"
  }

  trait ScalaClass extends TemplateBuilder with Extendible with Container {
    val caseClass = false
    val ports = new TemplateItems.TemplateList("Ports")

    lazy val port     = "(" ~ sep(ports.convert,",") ~ ")"
    lazy val creator = opt("case ", caseClass) ~ "class " ~ name ~ port ~ extendDec ~ classBody(items.convert) //" {\n\n " ~ indent(sep(items.convert,"\n")) ~ "\n}\n\n"
  }

  trait ScalaCaseClass extends ScalaClass {
    override val caseClass = true
  }

  trait ScalaMethod extends TemplateBuilder with Container {
    val ports = new TemplateItems.TemplateList("Ports")
    lazy val creator = "def " ~ name ~ "(" ~ sep(ports.convert,",") ~ ")" ~ " = " ~ "{" ~ indent(sep(items.convert,"\n")) ~ "}\n"
  }


}
