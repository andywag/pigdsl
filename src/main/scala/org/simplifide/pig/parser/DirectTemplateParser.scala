package org.simplifide.pig.parser

/**
 * Created by andy on 10/19/14.
 */

import org.simplifide.pig.PigTemplate
import org.simplifide.pig.model.PigModel
import org.simplifide.pig.parser.DirectTemplateParser.{ TemplateModel}
import org.simplifide.template.Template
import Template._
import PigTemplate._

trait DirectTemplateParser {

  def T(template:Template) = new TemplateModel(template)

  def concat(expressions:PigExpression*) = T("CONCAT " ~ paren(commaList(expressions.toList)))
  def size(expressions:PigExpression*)   = T("SIZE " ~   paren(commaList(expressions.toList)))
  def STAR                               = T("*")
  def GROUP                              = T("group")
  def AT                                 = T("@")
  def not(expr:PigExpression)            = T(" NOT " ~ paren(C(expr)))
  def EMPTY                              = T("")
  def Cast(e:PigExpression, n:String)    = T(s"($n)" ~ C(e))
  def Call(e:PigExpression, n:String)    = T(n ~ paren(C(e)))

  // Case Class Handling
  case class CaseClose(expr:PigExpression, clauses:List[PigExpression] = List()) extends PigModel {
    def when(expr:PigExpression) = new CaseOpen(this,expr)
    def Else(expr:PigExpression) = this.copy(clauses = clauses ::: List(T(" ELSE " ~ C(expr))))
  }
  case class CaseOpen(cas:CaseClose,when:PigExpression) {
    def then(expr:PigExpression) = cas.copy(clauses = cas.clauses ::: List(T(" WHEN " ~ C(when) ~ " THEN " ~ C(expr))))
  }




}

object DirectTemplateParser extends DirectTemplateParser {





  class TemplateModel(val template:Template) extends PigModel

  // Expressions
  def ::>(lhs:PigExpression, rhs:PigExpression)  = T( C(lhs) ~ "::" ~ C(rhs))
  def #>(lhs:PigExpression, rhs:PigExpression)  = T( C(lhs) ~ "#" ~ C(rhs))
  def -->(lhs:PigExpression, rhs:PigExpression) = T( C(lhs) ~ " .. "  ~ C(rhs))
  def ~>(lhs:PigExpression, rhs:PigExpression)  = T( C(lhs) ~ "."  ~ C(rhs))

}
