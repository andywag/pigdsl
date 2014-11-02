package org.simplifide.pig.test

import org.apache.pig.data.{DataType, Tuple}
import org.apache.pig.impl.logicalLayer.schema.Schema
import org.apache.pig.{EvalFunc, FuncSpec}
import org.simplifide.pig.parser.PigExpression
import org.simplifide.pig.user.UserDefined

/**
 * Created by andy on 10/29/14.
 */
class TestFunction extends EvalFunc[String] with UserDefined {

  val functionName = "TestFunction"
  def exec(input:Tuple): String = {
    if (input == null || input.size() == 0 ) ""
    else input.get(0).toString.toUpperCase;
  }
  def createFunc = new FuncSpec(this.getClass.getName,new Schema(new Schema.FieldSchema(null,DataType.CHARARRAY)))

}

object TestFunction {
  def apply(expressions:PigExpression*) = new UserDefined.UserDefinedFunction(new TestFunction(),expressions.toList)
}


