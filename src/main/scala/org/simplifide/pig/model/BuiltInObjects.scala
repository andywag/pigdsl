package org.simplifide.pig.model

import org.simplifide.pig.parser.PigExpression

/**
 * Created by andy on 10/26/14.
 */
object BuiltInObjects {

  class Base(val fName:String, val expr:PigExpression) extends PigModel

  case class Avg(override val expr:PigExpression) extends Base("Avg",expr)
  // TODO : Need Template Addition -- Testing
  case class BagToString(val expr:PigExpression, delimiter:Option[Char]) extends PigModel
  case class IsEmpty(override val expr:PigExpression) extends Base("IsEmpty",expr)
  case class Count(override val expr:PigExpression) extends Base("COUNT",expr)
  case class Sum(override val expr:PigExpression) extends Base("SUM",expr)

}
