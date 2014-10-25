package org.simplifide.pig.parser

import org.simplifide.pig.model.{PigObjects, PigModel}

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


  def ==(rhs:PigExpression) = new PigExpression.Binary("==",this,rhs)
  def ===(rhs:PigExpression) = new PigExpression.Binary("==",this,rhs)
  def !=(rhs:PigExpression) = new PigExpression.Binary("!=",this,rhs)
  def >(rhs:PigExpression) = new PigExpression.Binary(">",this,rhs)
  def <(rhs:PigExpression) = new PigExpression.Binary("<",this,rhs)
  def >=(rhs:PigExpression) = new PigExpression.Binary(">=",this,rhs)
  def <=(rhs:PigExpression) = new PigExpression.Binary("<=",this,rhs)
  def ->(rhs:PigExpression)  = new PigExpression.Arrow(this,rhs)

  def ??(rhs:PigExpression) = new PigExpression.QuestionOpen(this,rhs)
  def :: (rhs:PigExpression.QuestionOpen) = new PigExpression.QuestionClose(rhs.lhs,rhs.tr,this)

  def isNull     = new PigExpression.IsNull(this)
  def isNotNull  = new PigExpression.IsNotNull(this)

  // TODO : Need Test for this Operator (: is going to cause an issue)
  def ::>(rhs:PigExpression)                  = DirectTemplateParser.::>(this,rhs)
  // TODO : Need Test for this Operator
  def #>(rhs:PigExpression)                   = DirectTemplateParser.#>(this,rhs)
  def -->(rhs:PigExpression)                  = DirectTemplateParser.-->(this,rhs)
  def ~>(rhs:PigExpression)                   = DirectTemplateParser.~>(this,rhs)

  // Expression Function
  def as(expr:PigExpression)      = new PigObjects.As(this,expr)
  def matches(expr:PigExpression) = new PigObjects.Match(this,expr)
  // Unary Expressions
  def unary_- = new PigObjects.Negate(this)
  // Logical Operations
  def And(expr:PigExpression)     = new PigObjects.And(this,expr)
  def Or(expr:PigExpression)      = new PigObjects.Or(this,expr)

  def Left                        = new PigObjects.Left(this)
  def Right                       = new PigObjects.Right(this)
  def Full                        = new PigObjects.Full(this)

  def LeftOuter                   = new PigObjects.Left(this,true)
  def RightOuter                  = new PigObjects.Right(this,true)
  def FullOuter                   = new PigObjects.Full(this,true)

  def Asc                         = new PigObjects.Asc(this)
  def Desc                        = new PigObjects.Desc(this)

  def Otherwise                   = new PigObjects.Otherwise(this)


}

object PigExpression {

  case class IsNull(expression:PigExpression) extends PigModel
  case class IsNotNull(expression:PigExpression) extends PigModel

  case class Binary(op:String, lhs:PigExpression, rhs:PigExpression) extends PigModel
  case class Arrow(lhs:PigExpression,rhs:PigExpression) extends PigModel

  case class QuestionGroup(lhs:PigExpression, rhs:PigExpression) extends PigModel


  case class QuestionOpen(lhs:PigExpression, val tr:PigExpression) extends PigModel

  case class QuestionClose(lhs:PigExpression, val tr:PigExpression, val fa:PigExpression) extends PigModel

}
