package org.simplifide.pig.model


import org.apache.pig.StoreFuncInterface
import org.apache.pig.builtin.PigStorage
import org.simplifide.pig.model.Schema.BaseType
import org.simplifide.pig.parser.{BaseParser, PigExpression}

/**
 * Created by andy on 10/11/14.
 */
object PigObjects {

  case object NULL extends PigExpression with PigModel

  case class Dollar(val value:Int) extends PigExpression with PigModel {
    override val name = s"$value"
  }

  case class PigSymbol(val symbol:Symbol) extends PigExpression with PigModel {
    override val name = symbol.toString().substring(1)

    def := (rhs:PigExpression)(implicit parser:BaseParser) = {
      val ass = new Assign(this,rhs)
      parser.items.append(ass)
    }

    def by(rhs:PigExpression)  = new SymbolBy(this,rhs)
    def iff(rhs:PigExpression) = new IfExpression(this,rhs)
  }

  case class Flatten(val expr:PigExpression) extends PigExpression with PigModel
  case class IsEmpty(val expr:PigExpression) extends PigExpression with PigModel


  case class IfExpression(val lhs:PigExpression, val rhs:PigExpression) extends PigExpression with PigModel {

  }

  case class SymbolBy(val lhs:PigExpression, val rhs:PigExpression) extends PigExpression with PigModel {
    def left  = new SymbolByDirection(this,"left")
    def right = new SymbolByDirection(this,"right")
    def full  = new SymbolByDirection(this,"full")
    def outer = new SymbolByDirection(this,"outer")
  }
  case class SymbolByDirection(val symbol:SymbolBy, val text:String) extends PigExpression with PigModel {
    def outer = this.copy(text = this.text + " outer")
  }

  case class PigInt(val value:Int) extends PigExpression with PigModel {
    override val name = value.toString
  }

  case class Assign(val lhs:PigExpression, val rhs:PigExpression) extends PigExpression with PigModel

  case class Loader(val ident:String,
                    val usingModel:Option[String]=None,
                    val schema:Option[NewSchema]=None) extends PigModel with PigExpression {

    def using(value:String)               = copy(usingModel = Some(value))
    def as(schema:NewSchema)              = copy(schema = Some(schema))

  }

  case class Store(val input:PigExpression,
                   val intoModel:Option[String]=None,
                   val usingModel:Option[String]=None) extends PigExpression with PigModel {
    def into(value:String)                = copy(intoModel  = Some(value))
    def using(value:String)               = copy(usingModel = Some(value))
  }

  case class Order(val input:PigExpression) {
    def by(expressions:PigExpression*) = new OrderBy(input,expressions.toList)
  }

  case class OrderBy(val input:PigExpression, val inputs:List[PigExpression], 
                     val par:Option[PigExpression]= None) extends PigExpression with PigModel {
    def parallel(expression:PigExpression) = copy(par = Some(expression))
  }

  case class Rank(val input:PigExpression, val by:List[PigExpression]=List(), val denseO:Boolean = false) extends PigExpression with PigModel {
    def by(expressions:PigExpression*) = copy(by = this.by ::: expressions.toList)
    def dense = copy(denseO = true)
  }


  object All extends PigExpression with PigModel {
    override val name = "ALL"
  }

  /*
  case class Group(val input:PigExpression) extends PigExpression with PigModel{
    def all                            = new GroupBy(input,List(All))
    def by(expressions:PigExpression*) = new GroupBy(input,expressions.toList)
  }
  */


  class UsingObject(val value:String) extends PigExpression with PigModel

  case object Collected extends UsingObject("'collected'")
  case object Merge     extends UsingObject("'merge'")

  case class GroupBy(//val input:PigExpression,
                     val inputs:List[PigExpression],
                     val using:Option[UsingObject] = None,
                     val partitionBy:Option[String] = None,
                     val par:Option[PigExpression] = None) extends PigExpression with PigModel {

    def using(obj:UsingObject)         = copy(using = Some(obj))
    def partition(input:String)        = copy(partitionBy = Some(input))
    def parallel(input:PigExpression)  = copy(par = Some(input))
  }

  case class Cross(val inputs:List[PigExpression],
                   val partitionBy:Option[String] = None,
                   val par:Option[PigExpression] = None) extends PigExpression with PigModel {

  }

  case class Cube(val input:PigExpression) extends PigExpression with PigModel {
    def by(inputs:PigExpression*) = new CubeBy(input,inputs.toList)
  }

  case class CubeBy (val input:PigExpression,
                     val inputs:List[PigExpression],
                     val par:Option[PigExpression] = None) extends PigExpression with PigModel {
    def parallel(input:PigExpression)  = copy(par = Some(input))
  }


  trait CubeInput {val input:List[PigExpression]}
  case class CubeInner(val input:List[PigExpression]) extends PigExpression with PigModel with CubeInput
  case class RollUp(val input:List[PigExpression]) extends PigExpression with PigModel    with CubeInput


  case class Distinct(val input:PigExpression,
                      val partitionBy:Option[String] = None,
                      val par:Option[PigExpression] = None) extends PigExpression with PigModel {

    def partition(input:String)        = copy(partitionBy = Some(input))
    def parallel(input:PigExpression)  = copy(par = Some(input))

  }

  case class Dump(val input:PigExpression) extends PigExpression with PigModel

  case class LoaderSchema(val loader:Loader, val schema:Schema) extends PigModel

  case class Filter(val input:PigExpression) extends PigExpression with PigModel {
    def by(expr:PigExpression) = new FilterBy(input,expr)
  }

  case class Import(val input:String) extends PigExpression with PigModel


  class JoinUsing(val value:String) extends PigExpression with PigModel
  case object Replicated extends JoinUsing("'replicated'")
  case object Skewed extends JoinUsing("'skewed'")
  //case object Merge extends JoinUsing("'merge'")
  case object MergeSparse extends JoinUsing("'merge-sparse'")

  case class Join(val expressions:List[PigExpression])extends PigExpression with PigModel {
    def using(expr:PigExpression)      = new JoinBy(this,Some(expr))
    def partition(input:String)        = new JoinBy(this,None, Some(input))
    def parallel(input:PigExpression)  = new JoinBy(this,None,None, Some(input))

  }
  case class JoinBy(val join:Join,
                    val using:Option[PigExpression] = None,
                    val partitionBy:Option[String] = None,
                    val par:Option[PigExpression] = None) extends PigExpression with PigModel {
    def using(input:PigExpression)     = copy(using = Some(input))
    def partition(input:String)        = copy(partitionBy = Some(input))
    def parallel(input:PigExpression)  = copy(par = Some(input))

  }


  case class FilterBy(val input:PigExpression, val byTerm:PigExpression) extends PigExpression with PigModel

  case class Limit(val input:PigExpression, val limit:PigExpression) extends PigExpression with PigModel
  case class Sample(val input:PigExpression, val limit:PigExpression) extends PigExpression with PigModel

  case class Split(val input:PigExpression) extends PigExpression with PigModel {
    def into(expressions:PigExpression*) = new SplitInto(input,expressions.toList)
  }

  case class SplitInto(val input:PigExpression, val expressions:List[PigExpression]) extends PigExpression with PigModel

  case class Stream(val inputs:List[PigExpression]) extends PigExpression with PigModel {
    def through(expr:PigExpression) = new StreamThrough(this,expr)
  }
  case class StreamThrough(val stream:Stream, val through:PigExpression, schema:Option[NewSchema] = None) extends PigExpression with PigModel {
    def as(schema:NewSchema)              = copy(schema = Some(schema))
  }

  case object UnionBase extends PigExpression with PigModel {
    def onschema = UnionOnSchema
    def apply(expressions:PigExpression*) = new Union(expressions.toList,false)
  }
  case object UnionOnSchema extends PigExpression with PigModel {
    def apply(expressions:PigExpression*) = new Union(expressions.toList,true)
  }

  case class Union(val expressions:List[PigExpression],val onSchema:Boolean) extends PigExpression with PigModel


  case class MapReduce(val value:String) extends PigExpression with PigModel {
    def store(expr:PigExpression) = new MapReduceStore(value, expr)
  }
  case class MapReduceStore(val jar:String, val store:PigExpression) extends PigExpression with PigModel{
    def into(location:String) = new MapReduceStoreInto(jar,store,location)
  }
  case class MapReduceStoreInto(val jar:String, val store:PigExpression, val into:String) extends PigExpression with PigModel{
    def using(func:String) = new MapReduceStoreUsing(jar,store,into,func)
  }
  case class MapReduceStoreUsing(val jar:String, val store:PigExpression, val into:String, val usingStore:String) extends PigExpression with PigModel{
    def load(input:String) = new MapReduceLoad(this,input)
  }
  case class MapReduceLoad(val store:MapReduceStoreUsing, val location:String) extends PigExpression with PigModel {
    def using(input:String) = new MapReduceLoadUsing(store,location,input)
  }
  case class MapReduceLoadUsing(val store:MapReduceStoreUsing, val location:String, val using:String) extends PigExpression with PigModel {
    def as(schema:NewSchema) = MapReduceSchema(this,schema)
  }
  case class MapReduceSchema(val using:MapReduceLoadUsing, val schema:NewSchema) extends PigExpression with PigModel


  case class ForEach(val input:PigExpression) extends PigExpression with PigModel {
    def apply(expressions:PigExpression*) = new ForEachApply(input,expressions.toList)
    def generate(expr:PigExpression*) = new ForEachGenerate(input, expr.toList, None)
  }

  case class ForEachApply(val input:PigExpression, val inputs:List[PigExpression]) extends PigExpression with PigModel

  case class ForEachGenerate(val input:PigExpression, val expr:List[PigExpression], val as:Option[NewSchema]) extends PigExpression with PigModel {
    def as(schema:NewSchema) = copy(as = Some(schema))
  }


}
