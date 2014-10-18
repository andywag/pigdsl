package org.simplifide.pig.test

import org.scalatest._
import org.simplifide.pig.parser.BaseParser

import scala.reflect.io.Path

/**
 * Created by andy on 10/18/14.
 */
abstract class BasicPigTest(val name:String, val desc:String, val expected:Option[Int]) extends FlatSpec with BaseParser {
  Path(TestConstants.tempResults).deleteRecursively()

  name should desc in {
    val context = this.runAll
    val result = context.results("b").map(_.hashCode)
    System.out.println("Result" + result)
    assert(result == expected)
  }
}
