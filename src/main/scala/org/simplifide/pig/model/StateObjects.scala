package org.simplifide.pig.model


import org.apache.pig.StoreFuncInterface
import org.apache.pig.builtin.PigStorage
import org.simplifide.parser.model.TopModel
import org.simplifide.parser.parser.TopParser
import org.simplifide.pig.model.ExpressionObjects.{IfExpression, PigAll}
import org.simplifide.pig.model.Schema.BaseType
import org.simplifide.pig.parser.{BaseParser, PigExpression}

/**
 * Created by andy on 10/11/14.
 */
object StateObjects {


  /** Assert Method */
  case class Assert( lhs:PigExpression) extends PigModel {
    def by(expr:PigExpression) = new AssertBy(lhs,expr)
    def by(expr:PigExpression, message:String) = new AssertBy(lhs,expr,Some(message))
  }
  case class AssertBy(lhs:PigExpression, rhs:PigExpression, message:Option[String] = None) extends PigModel
  /** Cross Operation */
  case class Cross( inputs:List[PigExpression],
                    partitionBy:Option[String] = None,
                    par:Option[PigExpression] = None) extends PigModel
  /** Cube Operation */
  case class Cube( input:PigExpression) extends PigModel {
    def by(inputs:PigExpression*) = new CubeBy(input,inputs.toList)
  }

  case class CubeBy ( input:PigExpression,
                      inputs:List[PigExpression],
                      par:Option[PigExpression] = None) extends PigModel {
    def parallel(input:PigExpression)  = copy(par = Some(input))
  }
  trait CubeInput { val input:List[PigExpression]}
  case class CubeInner( input:List[PigExpression]) extends PigModel with CubeInput
  case class RollUp( input:List[PigExpression]) extends PigModel    with CubeInput
  /** Distinct Operation */
  case class Distinct( input:PigExpression,
                       partitionBy:Option[String] = None,
                       par:Option[PigExpression] = None) extends PigModel {

    def partition(input:String)        = copy(partitionBy = Some(input))
    def parallel(input:PigExpression)  = copy(par = Some(input))
  }
  /** Dump Operation */
  case class Dump( input:PigExpression) extends PigModel
  /** Filter Operation */
  case class Filter( input:PigExpression) extends PigModel {
    def by(expr:PigExpression) = new FilterBy(input,expr)
  }
  /** For Each Opeartion */
  case class ForEach(input:PigExpression) extends PigModel {
    def apply(expressions:PigExpression*) = new ForEachApply(input,expressions.toList)
    def generate(expr:PigExpression*) = new ForEachGenerate(input, expr.toList, None)
  }
  case class ForEachApply(input:PigExpression, inputs:List[PigExpression]) extends PigModel
  case class ForEachGenerate(input:PigExpression,expr:List[PigExpression],as:Option[PigExpression]) extends PigModel
  /** Group Operation */
  case class GroupBy(
                       inputs:List[PigExpression],
                       using:Option[UsingObject] = None,
                       partitionBy:Option[String] = None,
                       par:Option[PigExpression] = None) extends PigModel {

    def using(obj:UsingObject)         = copy(using = Some(obj))
    def partition(input:String)        = copy(partitionBy = Some(input))
    def parallel(input:PigExpression)  = copy(par = Some(input))
  }
  /** Import Operation */
  case class Import( input:String) extends PigModel
  /** Join Operation */
  case class Join( expressions:List[PigExpression])extends PigModel {
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
  /** Limit Operation */
  case class Limit( input:PigExpression,  limit:PigExpression) extends PigModel
  /** Load Operation */
  case class Loader( ident:String,
                     usingModel:Option[String]=None,
                     schema:Option[NewSchema]=None) extends PigModel with PigExpression {

    def using(value:String)               = copy(usingModel = Some(value))
    def as(schema:NewSchema)              = copy(schema = Some(schema))
  }
  /** Order By Operation */
  case class Order( input:PigExpression) {
    def by(expressions:PigExpression*) = new OrderBy(input,expressions.toList)
  }

  case class OrderBy( input:PigExpression,  inputs:List[PigExpression],
                      par:Option[PigExpression]= None) extends PigModel {
    def parallel(expression:PigExpression) = copy(par = Some(expression))
  }
  /** Rank Operation */
  case class Rank( input:PigExpression,  by:List[PigExpression]=List(),  denseO:Boolean = false) extends PigModel {
    def by(expressions:PigExpression*) = copy(by = this.by ::: expressions.toList)
    def dense = copy(denseO = true)
  }
  /** Sample Operation */
  case class Sample( input:PigExpression,  limit:PigExpression) extends PigModel
  /** Split Operation */
  case class Split( input:PigExpression) extends PigModel {
    def into(expressions:PigExpression*) = new SplitInto(input,expressions.toList)
  }
  case class SplitInto( input:PigExpression,  expressions:List[PigExpression]) extends PigModel
  /** Store Operation */
  case class Store( input:PigExpression,
                    intoModel:Option[String]=None,
                    usingModel:Option[String]=None) extends PigModel {
    def into(value:String)                = copy(intoModel  = Some(value))
    def using(value:String)               = copy(usingModel = Some(value))
  }
  /** Stream Operation */
  case class Stream( inputs:List[PigExpression]) extends PigModel {
    def through(expr:PigExpression) = new StreamThrough(this,expr)
  }
  case class StreamThrough( stream:Stream,  through:PigExpression, schema:Option[NewSchema] = None) extends PigModel {
    def as(schema:NewSchema)              = copy(schema = Some(schema))
  }
  /** Union Operation */
  case object UnionBase extends PigModel {
    def onschema = UnionOnSchema
    def apply(expressions:PigExpression*) = new Union(expressions.toList,false)
  }
  case object UnionOnSchema extends PigModel {
    def apply(expressions:PigExpression*) = new Union(expressions.toList,true)
  }
  case class Union( expressions:List[PigExpression], onSchema:Boolean) extends PigModel

  //
  case object NULL extends PigModel

  // Register Jar Class
  case class RegisterJar(value:String) extends PigModel



  /*
  case class Group( input:PigExpression) extends PigModel{
    def all                            = new GroupBy(input,List(All))
    def by(expressions:PigExpression*) = new GroupBy(input,expressions.toList)
  }
  */


  class UsingObject( value:String) extends PigModel

  case object Collected extends UsingObject("'collected'")
  case object Merge     extends UsingObject("'merge'")











  case class LoaderSchema( loader:Loader,  schema:Schema) extends PigModel





  class JoinUsing(val value:String) extends PigModel
  case object Replicated extends JoinUsing("'replicated'")
  case object Skewed extends JoinUsing("'skewed'")
  //case object Merge extends JoinUsing("'merge'")
  case object MergeSparse extends JoinUsing("'merge-sparse'")




  case class FilterBy( input:PigExpression,  byTerm:PigExpression) extends PigModel









  case class MapReduce( value:String) extends PigModel {
    def store(expr:PigExpression) = new MapReduceStore(value, expr)
  }
  case class MapReduceStore( jar:String,  store:PigExpression) extends PigModel{
    def into(location:String) = new MapReduceStoreInto(jar,store,location)
  }
  case class MapReduceStoreInto( jar:String,  store:PigExpression,  into:String) extends PigModel{
    def using(func:String) = new MapReduceStoreUsing(jar,store,into,func)
  }
  case class MapReduceStoreUsing( jar:String,  store:PigExpression,  into:String,  usingStore:String) extends PigModel{
    def load(input:String) = new MapReduceLoad(this,input)
  }
  case class MapReduceLoad( store:MapReduceStoreUsing,  location:String) extends PigModel {
    def using(input:String) = new MapReduceLoadUsing(store,location,input)
  }
  case class MapReduceLoadUsing( store:MapReduceStoreUsing,  location:String,  using:String) extends PigModel {
    def as(schema:NewSchema) = MapReduceSchema(this,schema)
  }
  case class MapReduceSchema( using:MapReduceLoadUsing,  schema:NewSchema) extends PigModel





  case class Generate(expr:PigExpression) extends PigModel


  case class Direct(value:String) extends PigModel



}
