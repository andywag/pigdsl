package org.simplifide.pig.model

import org.simplifide.pig.model.Schema.BaseType

/**
 * Created by andy on 10/11/14.
 */
trait Schema {
  def items:List[(String,BaseType)]
}

object Schema {

  def apply(items:(String,BaseType)*) = new Impl(items.toList)
  case class Impl(val items:List[(String,BaseType)]) extends Schema

  trait BaseType
  case object Int
  case object String
}
