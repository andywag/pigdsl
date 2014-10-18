package org.simplifide.pig.parser

import org.simplifide.pig.model.PigModel

/**
 * Created by andy on 10/12/14.
 */
trait PigExpression {

  def create:PigModel

  def +(rhs:PigExpression) = new PigExpression.Binary("+",this,rhs)
  def -(rhs:PigExpression) = new PigExpression.Binary("-",this,rhs)
  def *(rhs:PigExpression) = new PigExpression.Binary("*",this,rhs)
  def /(rhs:PigExpression) = new PigExpression.Binary("/",this,rhs)
  def %(rhs:PigExpression) = new PigExpression.Binary("%",this,rhs)
  //def ??(rhs:PigExpression.QuestionGroup) = new PigExpression.QuestionClose(this,rhs.lhs,rhs.rhs)
  def ??(rhs:PigExpression) = new PigExpression.QuestionOpen(this,rhs)


  def ==(rhs:PigExpression) = new PigExpression.Binary("==",this,rhs)
  def ===(rhs:PigExpression) = new PigExpression.Binary("==",this,rhs)
  def !=(rhs:PigExpression) = new PigExpression.Binary("!=",this,rhs)
  def >(rhs:PigExpression) = new PigExpression.Binary(">",this,rhs)
  def <(rhs:PigExpression) = new PigExpression.Binary("<",this,rhs)
  def >=(rhs:PigExpression) = new PigExpression.Binary(">=",this,rhs)
  def <=(rhs:PigExpression) = new PigExpression.Binary("<=",this,rhs)

  def :: (rhs:PigExpression.QuestionOpen) = new PigExpression.QuestionClose(rhs.lhs,rhs.tr,this)
}

object PigExpression {

  case class Binary(op:String, lhs:PigExpression, rhs:PigExpression) extends PigExpression with PigModel

  case class QuestionGroup(lhs:PigExpression, rhs:PigExpression) extends PigExpression with PigModel


  case class QuestionOpen(lhs:PigExpression, val tr:PigExpression) extends PigExpression with PigModel

  case class QuestionClose(lhs:PigExpression, val tr:PigExpression, val fa:PigExpression) extends PigExpression with PigModel

}
