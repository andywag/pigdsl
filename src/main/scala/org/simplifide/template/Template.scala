package org.simplifide.template

import org.simplifide.template.Template.Repeater

/**
 * Created by andy.wagner on 9/18/2014.
 */
trait Template {

  /** Return the value of the template at this position */
  //def apply(index:Int):Option[Template] =

  def s(index:Int)                  = this
  def o(index:Int):Option[Template] = if (index == 0) Some(this) else None

  def create:String
  def create(indent:Int):String = "  " + create.replace("\n","\n  ")
  def empty                     = false

  def ~(input:Template)                               = new Template.And(this,input)
  def ??(condition:Boolean, input:Template)           = new Template.Qu(this,condition,input)
}

object Template   {
  implicit def StringToTemplate(value:String) = new StringValue(value)
  implicit def ListToTemplate(value:List[Template]) = new ListTemplate(value)

  def sep(input:Template,seperator:Template) = new Repeater(input,seperator)
  def commaSep(input:Template) = sep(input,",")

  def rep(input:Template)                   = new Template.Repeater(input,"")
  def indent(input:Template) = new Indent(input)

  def surround(input:Template, front:Template) = new Surround(input, front, front)
  def surround(input:Template, front:Template, back:Template) = new Surround(input, front, back)
  def curly(input:Template) = surround(input,"{","}")
  def paren(input:Template) = surround(input,"(",")")
  def brack(input:Template) = surround(input,"[","]")
  def parenComma(input:Template) = paren(commaSep(input))
  def qu(condition:Boolean, tr:Template, fa:Template) = new Question(condition,tr, fa)


  def opt(option:Option[Template])                 = new Template.Opt(option)
  def opt(template:Template, condition:Boolean)    = new Template.Opt(if (condition) Some(template) else None)


  def L(templates:List[Template]) = new ListTemplate(templates)

  case object Empty extends Template {
    def create = ""
  }

  case class StringValue(value:String) extends Template {
    def create = value
  }

  case class ListTemplate(input:List[Template]) extends Template {
    def create                        = input.map(_.create).toString()
    override def empty                = (input.size == 0)

    override def s(index:Int)         = input(index)
    override def o(index:Int)         = if (index < input.size) Some(input(index)) else None

  }

  case class And(l:Template,r:Template) extends Template {
    def create = l.create + r.create

    override def s(index:Int)         = if (index == 0) l else r.s(index -1)
    override def o(index:Int)         = if (index == 0) Some(l) else r.o(index -1)

  }

  case class Qu(input1:Template, condition:Boolean, input:Template) extends Template {
    def create = input1.create + (if (condition) input.create else "")
  }

  case class Question(input:Boolean, in1:Template, in2:Template) extends Template {
    def create = if (input) in1.create else in2.create
  }

  case class Repeater(val input:Template,val seperator:Template) extends Template {
    def create = {
      input match {
        case ListTemplate(List())   => ""
        case ListTemplate(input)    => input.reduceLeft(_~seperator~_).create
        case _                      => input.create
      }
    }
  }

  case class Indent(val input:Template) extends Template {
    def create = "  " + input.create.replace("\n","\n  ")
  }

  case class Generic(value:String) extends Template {
    def create = value
  }



  case class Opt(input:Option[Template]) extends Template {
    def create = input.map(_.create).getOrElse("")
  }

  case class Surround(input:Template,first:Template, last:Template) extends Template {
    def create = (first ~ input ~ last).create
  }

}
