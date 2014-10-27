package org.simplifide.parser.parser

import org.simplifide.pig.model.{ModelBase, PigModel}
import ModelBase.{IntModel, StringModel, DoubleModel}
import org.simplifide.parser.model.TopModel
import org.simplifide.pig.model.PigModel
import org.simplifide.pig.model.StateObjects.{PigSymbol}

import scala.collection.mutable.ListBuffer

/**
 * Created by andy on 10/25/14.
 */
trait TopParser {

  implicit val parser:TopParser = this


  val items = new ListBuffer[TopModel]()
  def ->(model:TopModel) = {
    items.append(model);
    model
  }




}
