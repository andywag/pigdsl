package org.simplifide.pig.model

import org.simplifide.parser.parser.TopParser
import org.simplifide.pig.model.ExpressionObjects.{PigAll, IfExpression}
import org.simplifide.pig.model.PigAlias.{SymbolBy, Assign}
import org.simplifide.pig.parser.PigExpression

/**
 * Created by andy on 10/26/14.
 */


trait PigAlias extends PigModel {
  /** Assignment Operation which adds the assignment to the current context */
  def := (rhs:PigExpression)(implicit parser:TopParser) = {
    val ass = new Assign(this,rhs)
    parser.items.append(ass)
  }
  /** Assignment Operation which doesn't add to teh current context */
  def ::= (rhs:PigExpression) = new Assign(this,rhs)


  def by(rhs:PigExpression)  = new SymbolBy(this,rhs)

  def iff(rhs:PigExpression) = new IfExpression(this,rhs)
  def Otherwise                   = ExpressionObjects.Otherwise(this)

  def all                    = new PigAll(this)
}

object PigAlias {

  case class PigSymbol(symbol:Symbol) extends PigAlias {
    override val name = symbol.toString().substring(1)
  }

  case class PigAliasName(override val name:String) extends PigAlias

  case class Assign(lhs:PigExpression,rhs:PigExpression) extends PigModel

  case class SymbolBy(lhs:PigExpression, rhs:PigExpression) extends PigModel {
    def left  = new SymbolByDirection(this,"left")
    def right = new SymbolByDirection(this,"right")
    def full  = new SymbolByDirection(this,"full")
    def outer = new SymbolByDirection(this,"outer")
  }
  case class SymbolByDirection( symbol:SymbolBy, text:String) extends PigModel {
    def outer = this.copy(text = this.text + " outer")
  }

  object All extends PigModel {
    override val name = "ALL"
  }

  case class Dollar(val value:Int) extends PigModel {
    override val name = s"$value"
  }

}
