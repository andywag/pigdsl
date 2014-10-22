package org.simplifide.pig.test

import org.scalatest._
import org.simplifide.pig.PigContext
import org.simplifide.pig.model.PigObjects.{PigAliasName, PigSymbol}
import org.simplifide.pig.parser.BaseParser
import org.simplifide.pig.test.TestConstants._

import scala.collection.mutable.ListBuffer
import scala.reflect.io.Path

/**
 * Created by andy on 10/18/14.
 */
abstract class BasicPigTest(val name:String, val desc:String, val expected:Option[Int]) extends FlatSpec with BaseParser {
  Path(TestConstants.tempResults).deleteRecursively()
  Path(TestConstants.tempResults1).deleteRecursively()

  //val checkItems = new ListBuffer[(String,Option[Int])]()

  name should desc in {
    val context = this.runAll
    val result = context.results("b").map(_.hashCode)
    System.out.println("Result" + result)
    assert(result == expected)
  }
}

abstract class BasicPigTest2(val name:String, val desc:String) extends FlatSpec with BaseParser {

  val checkItems = new ListBuffer[(String,Option[Int])]()
  def check(name:String, value:Option[Int]) = {
    checkItems.append((name,value))
  }

  def addCheck(value:(String,Option[Int]), index:Int): Unit = {
    Path(TestConstants.tempResults + index).deleteRecursively()
    ->(store(PigAliasName(value._1)) into(tempResults+index) using "PigStorage(' ')")
    ->(dump(PigAliasName(value._1)))
  }

  def checkItem(context:PigContext,value:(String,Option[Int])) = {
    val result = context.results(value._1).map(_.hashCode)
    assert(result == value._2)
  }

  name should desc in {
    checkItems.zipWithIndex.foreach(x => addCheck(x._1, x._2))
    val context = this.runAll
    checkItems.foreach(checkItem(context,_))
  }
}
