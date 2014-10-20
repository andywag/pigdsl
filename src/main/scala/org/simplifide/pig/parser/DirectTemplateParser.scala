package org.simplifide.pig.parser

/**
 * Created by andy on 10/19/14.
 */

import org.simplifide.pig.PigTemplate
import org.simplifide.pig.model.PigModel
import org.simplifide.pig.parser.DirectTemplateParser.TemplateModel
import org.simplifide.template.Template
import Template._
import PigTemplate._

trait DirectTemplateParser {

  def T(template:Template) = new TemplateModel(template)

  def concat(expressions:PigExpression*) = T("CONCAT " ~ paren(commaList(expressions.toList)))
  def size(expressions:PigExpression*)   = T("SIZE " ~   paren(commaList(expressions.toList)))

}

object DirectTemplateParser {

  class TemplateModel(val template:Template) extends PigModel
}
