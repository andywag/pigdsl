package org.simplifide.pig.parser

import org.simplifide.pig.model.ModelBase.{IntModel, StringModel, DoubleModel}
import org.simplifide.pig.model.PigAlias.PigSymbol

/**
 * Created by andy on 10/25/14.
 */
trait BasicTypeParser {
  implicit def DoubleToModel(value:Double)  = DoubleModel(value)
  implicit def StringToModel(value:String)  = StringModel(value)
  implicit def SymbolToModel(symbol:Symbol) = PigSymbol(symbol)
  implicit def IntToModel(value:Int)        = IntModel(value)

  def I(value:Int)     = IntToModel(value)


}
