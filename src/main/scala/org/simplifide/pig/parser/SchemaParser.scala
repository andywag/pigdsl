package org.simplifide.pig.parser

import org.simplifide.pig.model.SchemaObjects.{MapPig, Tuple, Bag}
import org.simplifide.pig.model.{ExpressionObjects, StateObjects}

/**
 * Created by andy on 10/25/14.
 */
trait SchemaParser {
  def T(values:PigExpression*)                 = Tuple(values.toList)
  def B(values:Tuple*)              = Bag(values.toList)
  def M(values:(PigExpression,PigExpression)*) =
    MapPig(values.toList.map(x =>new ExpressionObjects.Arrow(x._1,x._2)))
}
