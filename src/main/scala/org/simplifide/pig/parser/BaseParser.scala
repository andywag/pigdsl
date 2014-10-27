package org.simplifide.pig.parser

import org.simplifide.parser.model.TopModel
import org.simplifide.parser.parser.TopParser
import org.simplifide.pig.{PigContext, PigExec, PigTemplate}
import org.simplifide.pig.model.{PigAlias, ExpressionObjects, StateObjects, PigModel}
import org.simplifide.pig.model.StateObjects._
import org.apache.pig.LoadFunc

import scala.collection.mutable.ListBuffer

/**
 * Created by andy on 10/11/14.
 */


trait BaseParser extends TopParser with StatementParser with BasicTypeParser with ConstantParser
  with SchemaParser with DirectTemplateParser with BuiltInParser  {


  def $(input:Int) = new PigAlias.Dollar(input)



  // Unary Operator
  def Not(expr:PigExpression)                  = new ExpressionObjects.Not(expr)


  // Operations
  def flatten(expr:PigExpression)  = new ExpressionObjects.Flatten(expr)

  def Case(expr:PigExpression)     = new DirectTemplateParser.CaseClose(expr)
  def Case                         = new DirectTemplateParser.CaseClose(EMPTY)



  def text = items.map(PigTemplate.createTemplate(_)).map(_.create)
  def createText = text.mkString("\n")

  def generate(expr:PigExpression) = new StateObjects.Generate(expr)

  def runAll = {
    val context = new PigContext()
    items.foreach(x => {
      PigExec.runModel(x, context)
    })
    context
  }



}
