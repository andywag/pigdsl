package org.simplifide.pig.parser

import org.simplifide.pig.model.ModelBase.{CharModel, IntModel, StringModel, DoubleModel}
import org.simplifide.pig.model.PigAlias
import org.simplifide.pig.model.PigAlias.PigSymbol
import org.simplifide.pig.user.UserDefined

/**
 * Created by andy on 10/25/14.
 */
trait BasicTypeParser {

  implicit def DoubleToModel(value:Double)  = DoubleModel(value)
  implicit def StringToModel(value:String)  = StringModel(value)
  implicit def SymbolToModel(symbol:Symbol) = PigSymbol(symbol)
  implicit def IntToModel(value:Int)        = IntModel(value)
  implicit def CharToModel(value:Char)      = CharModel(value)

  implicit def UserDefinedToFunction(value:UserDefined) = new UserDefined.UserDefinedFunction(value)

  def $(input:Int) = new PigAlias.Dollar(input)
  def I(value:Int)     = IntToModel(value)


}
