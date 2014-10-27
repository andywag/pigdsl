package org.simplifide.pig.parser

import org.simplifide.pig.model.StateObjects
import org.simplifide.pig.model.StateObjects.Loader

/**
 * Created by andy on 10/26/14.
 */
trait StatementParser {
  def assert(expr:PigExpression)     = new StateObjects.Assert(expr)
  def cogroup(expr:PigExpression*)   = new StateObjects.GroupBy(expr.toList)
  def cross(expr:PigExpression*)     = new StateObjects.Cross(expr.toList)
  def cube(expr:PigExpression)       = new StateObjects.Cube(expr)
  def cubeI(expr:PigExpression*)   = new StateObjects.CubeInner(expr.toList)
  def rollup(expr:PigExpression*)  = new StateObjects.RollUp(expr.toList)
  // TODO : Need Define Definition
  def distinct(expr:PigExpression)   = new StateObjects.Distinct(expr)
  def dump(expr:PigExpression)     = new StateObjects.Dump(expr)
  def filter(expr:PigExpression)     = new StateObjects.Filter(expr)
  def foreach(expr:PigExpression)    = new StateObjects.ForEach(expr)
  def group(expr:PigExpression*)     = new StateObjects.GroupBy(expr.toList)            // Testing Required
  def Import(value:String)           = new StateObjects.Import(value)
  def join(expr:PigExpression*)      = new StateObjects.Join(expr.toList)
  def limit(expr:PigExpression, value:PigExpression) = new StateObjects.Limit(expr,value)
  def load(value:String)             = new Loader(value)
  // TODO : Need MapReduce Definition -- Testing
  def mapreduce(value:String)      = new StateObjects.MapReduce(value)
  def order(expr:PigExpression)    = new StateObjects.Order(expr)
  def rank(expr:PigExpression)     = new StateObjects.Rank(expr)
  // TODO : Need Definition for Register
  def sample(expr:PigExpression, value:PigExpression) = new StateObjects.Sample(expr,value)
  def split(expr:PigExpression)                      = new StateObjects.Split(expr)
  def store(expr:PigExpression)    = new StateObjects.Store(expr)
  // TODO : Stream Needs Testing
  def stream(expr:PigExpression*)  = new StateObjects.Stream(expr.toList)
  def union                        = StateObjects.UnionBase
  // TODO : UDF Needs Definition
}
