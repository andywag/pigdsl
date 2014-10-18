package org.simplifide.template

/**
 * Created by andy.wagner on 9/18/2014.
 */

import Template._

object ScalaTemplate {

  implicit def t2l(template:List[Template]) = new ListTemplate(template)

  def scalaFile(name:Template, imports:Template, items:Template) =
    "package\n\n" ~ name ~ sep(imports,"\n") ~ "\n" ~ sep(items,"\n") ~ "\n"

  def scalaObject(name:String, items:List[Template]) =
    "object " ~ name ~ "{\n" ~ indent(L(items)) ~ "\n}"

  def caseClass(name:String, ports:List[Template], values:List[Template], sup:List[Template]) = {
    "case class " ~ name ~ "(" ~ sep(L(ports), ",") ~ ")" ??(sup.size > 0, " extends " ~ sep(L(sup)," with ")) ?? (values.size > 0, "{\n" ~ "}")
  }

  def scalaTrait(name:String, items:List[Template],extend:Option[Template]) = {
    "trait " ~ name ~ extendsClause(extend) ~ " {\n" ~ indent(sep(L(items),"\n")) ~"\n}"
  }

  def extendsClause(extend:Option[Template]) = {
    opt(extend.map(x => " extends " ~ x))
  }

  def methodCall(name:String, items:List[Template]):Template = {
    name ~ "(" ~ sep(L(items),",") ~ ")"
  }

  case object Val extends TemplateConstructor {
    def creator(input:Template) = "val " ~ input.s(0) ~ ":" ~ input.s(1)
  }


  def $val(name:Template, typ:Template, default:Option[Template]=None) = {
    "val " ~ name ~ ":" ~ typ ~ opt(default)
  }

  def methodDec(name:Template,inputs:List[Template],body:List[Template]) = {
    "def " ~ name ~ "(" ~ sep(L(inputs),",") ~ ") = {\n" ~ indent(sep(L(body),"\n")) ~ "\n}"
  }

  def classBody(items:Template) = {
    opt("{" ~ indent(sep(items,"\n") ~ "}"),!items.empty)

  }



  def scalaImport(name:Template) = "import " ~ name
  def str(name:String):Template       = name





}
