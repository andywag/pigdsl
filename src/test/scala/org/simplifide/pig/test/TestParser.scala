package org.simplifide.pig.test

import org.apache.log4j.PropertyConfigurator
import org.apache.pig.backend.executionengine.ExecJob
import org.apache.pig.builtin.PigStorage
import org.apache.pig.{PigRunner, ExecType, PigServer}
import org.apache.pig.scripting.Pig
import org.simplifide.pig.PigExec
import org.simplifide.pig.model.{Schema => S, NewSchema}
import org.simplifide.pig.parser.BaseParser
import org.simplifide.pig.test.TestParser.TestSchema

import scala.collection.JavaConversions
import scala.reflect.io.Path
import scala.tools.nsc.io.File

/**
 * Created by andy on 10/11/14.
 */
class TestParser extends BaseParser {



}

object TestParser extends BaseParser{

  val testLocation = "/home/andy/test"
  val testStorage  = testLocation + "/results"
  val testFile     = "/home/andy/test/testPig.txt"

  object TestSchema extends NewSchema {
    val a  = item("a",NewSchema.Int)
    val b  = item("b",NewSchema.Int)
    val c  = item("c",NewSchema.Int)
  }

  val a  = 'a := load(TestParser.testFile) using "PigStorage(' ')" as TestSchema
  val aa = 'b := order ('a)  by (TestSchema.a.desc  ,TestSchema.b.desc) //parallel 10
  val b  =       store('b) into (TestParser.testStorage) using "PigStorage(' ')"
  val c  =       dump('b)

  val d = 'c := cube ('a) by (cubeI(TestSchema.a, TestSchema.b),rollup(TestSchema.a,TestSchema.b))
  val e = 'd := distinct('c)
  val f = 'f := filter('a) by (TestSchema.a === 10)
  //val g = 'g := group('a) by (TestSchema.a)
  val h = 'h := join ('a by TestSchema.a, 'b by TestSchema.a)
  val i = 'i := limit('h, 10)
  val j = 'j := rank('i) by TestSchema.a
  val k = 'k := sample('j,10)
  val l =       split('a) into ('d iff (TestSchema.a === 1), 'e iff (TestSchema.b === 1))
  val m = 'm := stream('a)
  val n = 'n := union.onschema('a,'b)

  val o = 'o := foreach('a)
  /*foreach('b) (
    'u := distinct('c),
    'k := distinct('b)
  )*/
  //val d  = 'c := cube ('a) by (cube (a,b), rollup(a,b))

  //val a = load(TestParser.testFile) using "PigStorage()" as TestSchema
  //val b = store(a) into (TestParser.testLocation) using "PigStorage()"

  //items.appendAll(List(a,aa,b,c,d,e,f,g,h,i,j,l))


  def createFile = {
    val result = List.tabulate(100)(x => (x,2*x,4*x)).map(x => s"${x._1} ${x._2} ${x._3}").mkString("\n")
    File(testFile).writeAll(result)
  }

  def main(args:Array[String]) = {

    Path(testStorage).deleteRecursively()
    this.runAll

    //createFile
    //val server = new PigServer(ExecType.LOCAL)
    //server.getPigContext.setLog4jProperties()

    //server.debugOn()


    //System.out.println("A" + context.)


    //PigExec.runCommand(s"""a = load '$testFile' using PigStorage(' ') as (b:int,c:int,d:int);""")
    //PigExec.server.printAliases();
    //val a = PigExec.server.store("a",testStorage);
    //val results = JavaConversions.asScalaIterator(a.getResults)
    //System.out.println("Length " + results.length)
    //JavaConversions.asScalaIterator(a.getResults).foreach(x => System.out.println(x))
    //results.foreach(x => System.out.println(x))

    //System.out.println("Here " + a.getResults)

    //server.dumpSchema("a")
    //server.explain("a",System.out)


    //this.runAll
    //PigExec.runCommand("B = FOREACH A GENERATE CONCAT('a:',(chararray)f1), CONCAT('b:',(chararray)f2), CONCAT('c:',(chararray)f3);")
    //PigExec.runCommand("DUMP B;")

    //PigRunner.run(List("A = LOAD '/home/andy/test/testPig.txt'  using PigStorage() as (a:int,b:int,c:int)").toArray)
    //val temp = "A = LOAD '/home/andy/test/testPig.txt'  using PigStorage() as (a:int,b:int,c:int)"



    System.out.println("Done")

  }
}
