package org.simplifide.pig.model

import org.simplifide.pig.parser.PigExpression

/**
 * Created by andy on 10/25/14.
 */
object SchemaObjects {

  case class Tuple(expressions:List[PigExpression])        extends PigModel
  case class Bag(expressions:List[Tuple])                  extends PigModel
  case class MapPig(expressions:List[ExpressionObjects.Arrow]) extends PigModel

}
