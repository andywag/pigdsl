package org.simplifide.pig.parser

import org.simplifide.parser.parser.Expression
import org.simplifide.pig.PigTemplate._
import org.simplifide.pig.model.{ExpressionObjects, StateObjects, PigModel}

import ExpressionObjects._

/**
 * Created by andy on 10/12/14.
 */
trait PigExpression extends Expression {

  def create:PigModel

  def +(rhs:PigExpression)   = Binary("+",this,rhs)
  def -(rhs:PigExpression)   = Binary("-",this,rhs)
  def *(rhs:PigExpression)   = Binary("*",this,rhs)
  def /(rhs:PigExpression)   = Binary("/",this,rhs)
  def %(rhs:PigExpression)   = Binary("%",this,rhs)
  def ==(rhs:PigExpression)  =  Binary("==",this,rhs)
  def ===(rhs:PigExpression) =  Binary("==",this,rhs)
  def !=(rhs:PigExpression)  =  Binary("!=",this,rhs)
  def >(rhs:PigExpression)   =  Binary(">",this,rhs)
  def <(rhs:PigExpression)   =  Binary("<",this,rhs)
  def >=(rhs:PigExpression)  =  Binary(">=",this,rhs)
  def <=(rhs:PigExpression)  =  Binary("<=",this,rhs)
  def ->(rhs:PigExpression)  =  Arrow(this,rhs)

  def ??(rhs:PigExpression) = QuestionOpen(this,rhs)
  def :: (rhs:QuestionOpen) = QuestionClose(rhs.lhs,rhs.tr,this)

  def isNull               = IsNull(this)
  def isNotNull            = IsNotNull(this)



  // TODO : Need Test for this Operator (: is going to cause an issue)
  def ::>(rhs:PigExpression)                  = Binary("::",this,rhs)
  // TODO : Need Test for this Operator
  def #>(rhs:PigExpression)                   = BinaryNoSpace("#",this,rhs)
  def -->(rhs:PigExpression)                  = BinaryNoSpace(" .. ",this,rhs)
  def ~>(rhs:PigExpression)                   = BinaryNoSpace(".",this,rhs)

  // Expression Function
  def as(expr:PigExpression)                  = As(this,expr)
  def matches(expr:PigExpression)             = Match(this,expr)
  // Unary Expressions
  def unary_-                     = ExpressionObjects.Negate(this)
  // Logical Operations
  def And(expr:PigExpression)     = ExpressionObjects.And(this,expr)
  def Or(expr:PigExpression)      = ExpressionObjects.Or(this,expr)

  // Unary Expressions for Special Cases
  def Left                        = ExpressionObjects.Left(this)
  def Right                       = ExpressionObjects.Right(this)
  def Full                        = ExpressionObjects.Full(this)

  def LeftOuter                   = ExpressionObjects.Left(this,true)
  def RightOuter                  = ExpressionObjects.Right(this,true)
  def FullOuter                   = ExpressionObjects.Full(this,true)

  def Asc                         = ExpressionObjects.Asc(this)
  def Desc                        = ExpressionObjects.Desc(this)



}

object PigExpression {


}
