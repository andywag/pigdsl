package org.simplifide.pig.model


import org.apache.pig.StoreFuncInterface
import org.apache.pig.builtin.PigStorage
import org.simplifide.pig.model.Schema.BaseType
import org.simplifide.pig.parser.{BaseParser, PigExpression}

/**
 * Created by andy on 10/11/14.
 */
object PigObjects {

  case object NULL extends PigModel

  case class Not(lhs:PigExpression) extends PigModel
  case class And(lhs:PigExpression, rhs:PigExpression) extends PigModel
  case class Or(lhs:PigExpression, rhs:PigExpression)  extends PigModel

  case class Negate(val expression:PigExpression) extends PigModel

  case class Tuple(val expressions:List[PigExpression])        extends PigModel
  case class Bag(val expressions:List[Tuple])                  extends PigModel
  case class MapPig(val expressions:List[PigExpression.Arrow]) extends PigModel

  case class Dollar(val value:Int) extends PigModel {
    override val name = s"$value"
  }

  case class PigString(override val name:String) extends PigModel
  case class PigDouble(val value:Double) extends PigModel

  trait PigAlias extends PigModel {
    def := (rhs:PigExpression)(implicit parser:BaseParser) = {
      val ass = new Assign(this,rhs)
      parser.items.append(ass)
    }

    def ::= (rhs:PigExpression) = new Assign(this,rhs)


    def by(rhs:PigExpression)  = new SymbolBy(this,rhs)
    def iff(rhs:PigExpression) = new IfExpression(this,rhs)
    def all                    = new PigAll(this)
  }

  case class PigSymbol(val symbol:Symbol) extends PigAlias {
    override val name = symbol.toString().substring(1)
  }

  case class PigAliasName(override val name:String) extends PigAlias

  case class PigAll(exr:PigExpression) extends PigModel

  case class Flatten(val expr:PigExpression) extends PigModel
  case class IsEmpty(val expr:PigExpression) extends PigModel


  case class IfExpression(val lhs:PigExpression, val rhs:PigExpression) extends PigModel {

  }

  case class SymbolBy(val lhs:PigExpression, val rhs:PigExpression) extends PigModel {
    def left  = new SymbolByDirection(this,"left")
    def right = new SymbolByDirection(this,"right")
    def full  = new SymbolByDirection(this,"full")
    def outer = new SymbolByDirection(this,"outer")
  }
  case class SymbolByDirection(val symbol:SymbolBy, val text:String) extends PigModel {
    def outer = this.copy(text = this.text + " outer")
  }

  case class PigInt(val value:Int) extends PigModel {
    override val name = value.toString
  }

  case class Assign(val lhs:PigExpression, val rhs:PigExpression) extends PigModel

  case class Assert(val lhs:PigExpression) extends PigModel {
    def by(expr:PigExpression) = new AssertBy(lhs,expr)
    def by(expr:PigExpression, message:String) = new AssertBy(lhs,expr,Some(message))
  }
  case class AssertBy(lhs:PigExpression, rhs:PigExpression, message:Option[String] = None) extends PigModel

  case class Loader(val ident:String,
                    val usingModel:Option[String]=None,
                    val schema:Option[NewSchema]=None) extends PigModel with PigExpression {

    def using(value:String)               = copy(usingModel = Some(value))
    def as(schema:NewSchema)              = copy(schema = Some(schema))

  }

  case class Store(val input:PigExpression,
                   val intoModel:Option[String]=None,
                   val usingModel:Option[String]=None) extends PigModel {
    def into(value:String)                = copy(intoModel  = Some(value))
    def using(value:String)               = copy(usingModel = Some(value))
  }

  case class Order(val input:PigExpression) {
    def by(expressions:PigExpression*) = new OrderBy(input,expressions.toList)
  }

  case class OrderBy(val input:PigExpression, val inputs:List[PigExpression], 
                     val par:Option[PigExpression]= None) extends PigModel {
    def parallel(expression:PigExpression) = copy(par = Some(expression))
  }

  case class Rank(val input:PigExpression, val by:List[PigExpression]=List(), val denseO:Boolean = false) extends PigModel {
    def by(expressions:PigExpression*) = copy(by = this.by ::: expressions.toList)
    def dense = copy(denseO = true)
  }


  object All extends PigModel {
    override val name = "ALL"
  }

  /*
  case class Group(val input:PigExpression) extends PigModel{
    def all                            = new GroupBy(input,List(All))
    def by(expressions:PigExpression*) = new GroupBy(input,expressions.toList)
  }
  */


  class UsingObject(val value:String) extends PigModel

  case object Collected extends UsingObject("'collected'")
  case object Merge     extends UsingObject("'merge'")

  case class GroupBy(//val input:PigExpression,
                     val inputs:List[PigExpression],
                     val using:Option[UsingObject] = None,
                     val partitionBy:Option[String] = None,
                     val par:Option[PigExpression] = None) extends PigModel {

    def using(obj:UsingObject)         = copy(using = Some(obj))
    def partition(input:String)        = copy(partitionBy = Some(input))
    def parallel(input:PigExpression)  = copy(par = Some(input))
  }

  case class Cross(val inputs:List[PigExpression],
                   val partitionBy:Option[String] = None,
                   val par:Option[PigExpression] = None) extends PigModel {

  }

  case class Cube(val input:PigExpression) extends PigModel {
    def by(inputs:PigExpression*) = new CubeBy(input,inputs.toList)
  }

  case class CubeBy (val input:PigExpression,
                     val inputs:List[PigExpression],
                     val par:Option[PigExpression] = None) extends PigModel {
    def parallel(input:PigExpression)  = copy(par = Some(input))
  }


  trait CubeInput {val input:List[PigExpression]}
  case class CubeInner(val input:List[PigExpression]) extends PigModel with CubeInput
  case class RollUp(val input:List[PigExpression]) extends PigModel    with CubeInput


  case class Distinct(val input:PigExpression,
                      val partitionBy:Option[String] = None,
                      val par:Option[PigExpression] = None) extends PigModel {

    def partition(input:String)        = copy(partitionBy = Some(input))
    def parallel(input:PigExpression)  = copy(par = Some(input))

  }

  case class Dump(val input:PigExpression) extends PigModel

  case class LoaderSchema(val loader:Loader, val schema:Schema) extends PigModel

  case class Filter(val input:PigExpression) extends PigModel {
    def by(expr:PigExpression) = new FilterBy(input,expr)
  }

  case class Import(val input:String) extends PigModel


  class JoinUsing(val value:String) extends PigModel
  case object Replicated extends JoinUsing("'replicated'")
  case object Skewed extends JoinUsing("'skewed'")
  //case object Merge extends JoinUsing("'merge'")
  case object MergeSparse extends JoinUsing("'merge-sparse'")

  case class Join(val expressions:List[PigExpression])extends PigModel {
    def using(expr:PigExpression)      = new JoinBy(this,Some(expr))
    def partition(input:String)        = new JoinBy(this,None, Some(input))
    def parallel(input:PigExpression)  = new JoinBy(this,None,None, Some(input))

  }
  case class JoinBy(join:Join,
                    using:Option[PigExpression] = None,
                    partitionBy:Option[String] = None,
                    par:Option[PigExpression] = None) extends PigModel {
    def using(input:PigExpression)     = copy(using = Some(input))
    def partition(input:String)        = copy(partitionBy = Some(input))
    def parallel(input:PigExpression)  = copy(par = Some(input))

  }


  case class FilterBy(val input:PigExpression, val byTerm:PigExpression) extends PigModel

  case class Limit(val input:PigExpression, val limit:PigExpression) extends PigModel
  case class Sample(val input:PigExpression, val limit:PigExpression) extends PigModel

  case class Split(val input:PigExpression) extends PigModel {
    def into(expressions:PigExpression*) = new SplitInto(input,expressions.toList)
  }

  case class SplitInto(val input:PigExpression, val expressions:List[PigExpression]) extends PigModel

  case class Stream(val inputs:List[PigExpression]) extends PigModel {
    def through(expr:PigExpression) = new StreamThrough(this,expr)
  }
  case class StreamThrough(val stream:Stream, val through:PigExpression, schema:Option[NewSchema] = None) extends PigModel {
    def as(schema:NewSchema)              = copy(schema = Some(schema))
  }

  case object UnionBase extends PigModel {
    def onschema = UnionOnSchema
    def apply(expressions:PigExpression*) = new Union(expressions.toList,false)
  }
  case object UnionOnSchema extends PigModel {
    def apply(expressions:PigExpression*) = new Union(expressions.toList,true)
  }

  case class Union(val expressions:List[PigExpression],val onSchema:Boolean) extends PigModel


  case class MapReduce(val value:String) extends PigModel {
    def store(expr:PigExpression) = new MapReduceStore(value, expr)
  }
  case class MapReduceStore(val jar:String, val store:PigExpression) extends PigModel{
    def into(location:String) = new MapReduceStoreInto(jar,store,location)
  }
  case class MapReduceStoreInto(val jar:String, val store:PigExpression, val into:String) extends PigModel{
    def using(func:String) = new MapReduceStoreUsing(jar,store,into,func)
  }
  case class MapReduceStoreUsing(val jar:String, val store:PigExpression, val into:String, val usingStore:String) extends PigModel{
    def load(input:String) = new MapReduceLoad(this,input)
  }
  case class MapReduceLoad(val store:MapReduceStoreUsing, val location:String) extends PigModel {
    def using(input:String) = new MapReduceLoadUsing(store,location,input)
  }
  case class MapReduceLoadUsing(val store:MapReduceStoreUsing, val location:String, val using:String) extends PigModel {
    def as(schema:NewSchema) = MapReduceSchema(this,schema)
  }
  case class MapReduceSchema(val using:MapReduceLoadUsing, val schema:NewSchema) extends PigModel


  case class ForEach(val input:PigExpression) extends PigModel {
    def apply(expressions:PigExpression*) = new ForEachApply(input,expressions.toList)
    def generate(expr:PigExpression*) = new ForEachGenerate(input, expr.toList, None)
  }

  case class ForEachApply(val input:PigExpression, val inputs:List[PigExpression]) extends PigModel

  case class ForEachGenerate(val input:PigExpression, val expr:List[PigExpression], val as:Option[PigExpression]) extends PigModel {
    //def as(schema:PigExpression) = copy(as = Some(schema))
  }

  case class As(lhs:PigExpression, input:PigExpression) extends PigModel
  case class Match(lhs:PigExpression, input:PigExpression) extends PigModel

  case class Generate(expr:PigExpression) extends PigModel

  // TODO Convert to Single Class
  case class Left(expr:PigExpression, outer:Boolean = false) extends PigModel
  case class Right(expr:PigExpression, outer:Boolean = false) extends PigModel
  case class Full(expr:PigExpression, outer:Boolean = false) extends PigModel

  case class Outer(expr:PigExpression) extends PigModel

  case class Asc(expr:PigExpression) extends PigModel
  case class Desc(expr:PigExpression) extends PigModel

  case class Otherwise(expr:PigExpression) extends PigModel


}
