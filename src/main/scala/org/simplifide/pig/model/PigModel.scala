package org.simplifide.pig.model

import org.simplifide.parser.model.TopModel
import org.simplifide.pig.parser.PigExpression
import org.simplifide.pig.{PigExec, PigTemplate, PigResult}


/**
 * Created by andy on 10/11/14.
 */
trait PigModel extends PigExpression with TopModel {
  val name:String = "NA"
  def create:PigModel = this
}

object PigModel {



}
