package org.simplifide.template

import scala.collection.mutable.ListBuffer

/**
 * Created by andy.wagner on 9/19/2014.
 */
trait TemplateBuilder extends Template {
  val name:String
  val creator:Template

  def create = creator.create

  // Set of Values for the Generated Class
  val values = new ListBuffer[TemplateBuilder]()
  def value[T <: TemplateBuilder](item:T) = {values.append(item); item}

  // Set of Inputs for the Generated Class
  val inputs = new ListBuffer[TemplateBuilder]()
  def input[T <: TemplateBuilder](item:T) = {inputs.append(item); item}

  /** Creates the Builder Class Associated with this template */
  def construct = {
    val valueDec   = values.map(x => ScalaTemplate.$val(x.name,"Template")).toList
    val bufferDec  = inputs.map(x => ScalaTemplate.$val(x.name,"ListBuffer[Template]",Some(" = new ListBuffer[Template]()"))).toList
    val methodDec  = {
      def methodSignal(x:TemplateBuilder) = {
        val methodName = x.name.substring(0,x.name.length-1)
        val methodStatements = List(s"${x.name}.append(input)", "input").map(ScalaTemplate.str(_))
        ScalaTemplate.methodDec(methodName,List("input:Template"),methodStatements)
      }
      inputs.map(x => methodSignal(x)).toList
    }
    ScalaTemplate.scalaTrait(name,valueDec ::: bufferDec ::: methodDec,Some("temp")).create
  }
}

object TemplateBuilder {

}
