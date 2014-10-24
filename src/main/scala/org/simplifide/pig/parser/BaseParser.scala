package org.simplifide.pig.parser

import org.simplifide.pig.{PigContext, PigExec, PigTemplate}
import org.simplifide.pig.model.{PigObjects, PigModel}
import org.simplifide.pig.model.PigObjects._
import org.apache.pig.LoadFunc

import scala.collection.mutable.ListBuffer

/**
 * Created by andy on 10/11/14.
 */


trait BaseParser extends DirectTemplateParser  {


  implicit def Double2Pig(value:Double)  = PigDouble(value)
  implicit def String2Pig(value:String)  = PigString(value)
  implicit def Symbol2Pig(symbol:Symbol) = PigSymbol(symbol)
  implicit def Int2Pit(value:Int)        = PigInt(value)

  val items = new ListBuffer[PigModel]()
  def ->(model:PigModel) = {
    items.append(model);
    model
  }
  def $(input:Int) = new PigObjects.Dollar(input)

  val NULL = PigObjects.NULL

  implicit val parser:BaseParser = this


  def T(values:PigExpression*)                 = new PigObjects.Tuple(values.toList)
  def B(values:PigObjects.Tuple*)              = new PigObjects.Bag(values.toList)
  def M(values:(PigExpression,PigExpression)*) =
    new PigObjects.MapPig(values.toList.map(x =>new PigExpression.Arrow(x._1,x._2)))

  def assert(expr:PigExpression)   = new PigObjects.Assert(expr)
  def load(value:String)           = new Loader(value)
  def store(expr:PigExpression)    = new PigObjects.Store(expr)
  def dump(expr:PigExpression)     = new PigObjects.Dump(expr)
  def order(expr:PigExpression)    = new PigObjects.Order(expr)
  def group(expr:PigExpression*)    = new PigObjects.GroupBy(expr.toList)            // Testing Required
  def cogroup(expr:PigExpression*)  = new PigObjects.GroupBy(expr.toList)            // Testing Required
  def cross(expr:PigExpression*)   = new PigObjects.Cross(expr.toList)     // Testing Required
  def cube(expr:PigExpression)     = new PigObjects.Cube(expr)
  def cubeI(expr:PigExpression*)   = new PigObjects.CubeInner(expr.toList)
  def rollup(expr:PigExpression*)  = new PigObjects.RollUp(expr.toList)
  def distinct(expr:PigExpression) = new PigObjects.Distinct(expr)
  def filter(expr:PigExpression)   = new PigObjects.Filter(expr)
  def Import(value:String)         = new PigObjects.Import(value)
  def join(expr:PigExpression*)     = new PigObjects.Join(expr.toList)
  def limit(expr:PigExpression, value:PigExpression) = new PigObjects.Limit(expr,value)
  def sample(expr:PigExpression, value:PigExpression) = new PigObjects.Sample(expr,value)
  def split(expr:PigExpression)                      = new PigObjects.Split(expr)
  def stream(expr:PigExpression*)  = new PigObjects.Stream(expr.toList)
  def union                        = PigObjects.UnionBase
  def mapreduce(value:String)      = new PigObjects.MapReduce(value)

  def rank(expr:PigExpression)     = new PigObjects.Rank(expr)
  def foreach(expr:PigExpression)  = new PigObjects.ForEach(expr)
  // Operations
  def flatten(expr:PigExpression)  = new PigObjects.Flatten(expr)
  def isEmpty(expr:PigExpression)  = new PigObjects.IsEmpty(expr)
  def Case(expr:PigExpression)     = new DirectTemplateParser.CaseClose(expr)
  def Case                         = new DirectTemplateParser.CaseClose(EMPTY)
  // Casting Operators
  def bag(expr:PigExpression)      = DirectTemplateParser.Cast(expr,"bag")
  def tuple(expr:PigExpression)    = DirectTemplateParser.Cast(expr,"tuple")
  def int(expr:PigExpression)      = DirectTemplateParser.Cast(expr,"int")
  def long(expr:PigExpression)      = DirectTemplateParser.Cast(expr,"long")
  def float(expr:PigExpression)    = DirectTemplateParser.Cast(expr,"float")
  def double(expr:PigExpression)      = DirectTemplateParser.Cast(expr,"double")
  def chararray(expr:PigExpression)      = DirectTemplateParser.Cast(expr,"chararray")
  def bytearray(expr:PigExpression)    = DirectTemplateParser.Cast(expr,"bytearray")
  def boolean(expr:PigExpression)      = DirectTemplateParser.Cast(expr,"boolean")
  // Standard Functions
  def sum(expr:PigExpression)       = DirectTemplateParser.Call(expr,"SUM")
  def count(expr:PigExpression)     = DirectTemplateParser.Call(expr,"COUNT")

  def text = items.map(PigTemplate.createTemplate(_)).map(_.create)
  def createText = text.mkString("\n")

  def runAll = {
    val context = new PigContext()
    items.foreach(x => {
      //System.out.println(x)
      PigExec.runModel(x, context)
    })
    context
  }



}
