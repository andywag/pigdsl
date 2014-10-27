package org.simplifide.pig.parser

import org.simplifide.pig.model.BuiltInObjects._
import org.simplifide.pig.model.StateObjects

/**
 * Created by andy on 10/26/14.
 */
trait BuiltInParser {

  // Standard Functions
  def avg(expr:PigExpression)       = Avg(expr)
  def bagToString(expr:PigExpression, delimiter:Option[Character] = None) = BagToString(expr,delimiter),
  def isEmpty(expr:PigExpression)   = IsEmpty(expr)
  def sum(expr:PigExpression)       = Sum(expr)
  def count(expr:PigExpression)     = Count(expr)
}
