package org.simplifide.pig.user

import org.apache.pig.FuncSpec
import org.simplifide.pig.model.PigModel
import org.simplifide.pig.parser.PigExpression

/**
 * Created by andy on 10/30/14.
 */
trait UserDefined extends PigModel {

  val functionName:String
  def createFunc:FuncSpec

  /*def functionName = {
    val values = this.createFunc.getClassName.split(".")
    values(values.length-1)
  }*/

}

object UserDefined {

  case class UserDefinedFunction(function:UserDefined, expressions:List[PigExpression]=List()) extends PigModel {
    def apply(expressions:PigExpression*) = copy(expressions = expressions.toList)

    def functionName = {
      val func   = function.createFunc
      val values = func.getClassName.split("\\.")
      values(values.length-1)
    }

  }

}
