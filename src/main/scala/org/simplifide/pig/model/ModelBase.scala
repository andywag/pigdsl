package org.simplifide.pig.model

/**
 * Created by andy on 10/25/14.
 */
object ModelBase {

  case class StringModel(override val name:String) extends PigModel
  case class DoubleModel(value:Double)             extends PigModel
  case class IntModel(value:Int)                   extends PigModel {
    override val name = value.toString
  }
  case class CharModel(value:Char)                 extends PigModel

}
