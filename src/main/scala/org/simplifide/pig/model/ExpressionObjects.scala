package org.simplifide.pig.model

import org.simplifide.pig.PigTemplate._
import org.simplifide.pig.parser.{DirectTemplateParser, PigExpression}

/**
 * Created by andy on 10/25/14.
 */
object ExpressionObjects {

  case class Negate(lhs:PigExpression) extends PigModel
  case class Not(lhs:PigExpression) extends PigModel
  // All Binary Operators
  case class Binary(op:String, lhs:PigExpression, rhs:PigExpression) extends PigModel
  case class BinaryNoSpace(op:String, lhs:PigExpression, rhs:PigExpression) extends PigModel

  case class And(lhs:PigExpression, rhs:PigExpression) extends PigModel
  case class Or(lhs:PigExpression, rhs:PigExpression)  extends PigModel
  //
  case class IsNull(lhs:PigExpression) extends PigModel
  case class IsNotNull(lhs:PigExpression) extends PigModel
  // Operator from ->
  case class Arrow(lhs:PigExpression,rhs:PigExpression) extends PigModel
  // Question Operators
  case class QuestionGroup(lhs:PigExpression, rhs:PigExpression) extends PigModel
  case class QuestionOpen(lhs:PigExpression,  tr:PigExpression) extends PigModel
  case class QuestionClose(lhs:PigExpression, tr:PigExpression,fa:PigExpression) extends PigModel


  case class As(lhs:PigExpression, input:PigExpression) extends PigModel
  case class Match(lhs:PigExpression, input:PigExpression) extends PigModel

  // TODO Convert to Single Class
  case class Left(expr:PigExpression, outer:Boolean = false) extends PigModel
  case class Right(expr:PigExpression, outer:Boolean = false) extends PigModel
  case class Full(expr:PigExpression, outer:Boolean = false) extends PigModel
  // Ascending/Descendign for Rank
  case class Asc(expr:PigExpression) extends PigModel
  case class Desc(expr:PigExpression) extends PigModel
  // Otherwise for If Clause

  // All Operator
  case class PigAll(expr:PigExpression) extends PigModel

  // Flatten Operator
  case class Flatten(val expr:PigExpression) extends PigModel
  // If Expression
  case class IfExpression(lhs:PigExpression, rhs:PigExpression) extends PigModel
  case class Otherwise(expr:PigExpression) extends PigModel


}
