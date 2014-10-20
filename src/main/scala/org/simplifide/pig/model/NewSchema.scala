package org.simplifide.pig.model

import org.simplifide.pig.parser.PigExpression

import scala.collection.mutable.ListBuffer

/**
 * Created by andy on 10/11/14.
 */
trait NewSchema  {

  /** List of Items Contained in this Schema */
  val newItems = new ListBuffer[NewSchema.Element]()

  def item(name:String, typ:NewSchema.Type, parent:Option[NewSchema.Element]=None) = {
    val it = new NewSchema.Pair(name,typ, parent)
    newItems.append(it)
    it
  }

}

object NewSchema {

  trait TupleTrait extends NewSchema with NewSchema.Type with Element {
    def $(value:Int) = new TupleDollar(this,value)
    def item(name:String, typ:NewSchema.Type) = {
      super.item(name,typ,Some(this))
    }
  }

  case class TupleDollar(tra:TupleTrait,val value:Int) extends NewSchema with NewSchema.Type with Element {
    override val name = tra.name + ".$" + value
    override def complexName = tra.complexName + ".$" + value
  }

  case class Tuple(override val name:String, val parent:Option[Element] = None) extends TupleTrait {
    override def complexName = parent.map(_.complexName + ".").getOrElse("") + name
  }

  case class TupleSchema(val schema:NewSchema) extends TupleTrait {}
  
  
  trait Element extends PigModel {
    def complexName = name
    def asc  = new Ascending(this)
    def desc = new Descending(this)
  }

  case class Ascending(val value:PigExpression)   extends PigModel {
    override val name = value.create.name + " asc"
  }
  case class Descending(val value:PigExpression)  extends PigModel {
    override val name = value.create.name + " desc"
  }

  case class Pair(override val name:String, val typ:NewSchema.Type, val parent:Option[Element]) extends Element {
    override def complexName = parent.map(_.complexName + ".").getOrElse("") + name
  }

  trait Type
  case object Int    extends Type
  case object String extends Type
  case object Float  extends Type
  //case object Tuple  extends Type



}
