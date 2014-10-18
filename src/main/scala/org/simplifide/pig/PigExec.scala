package org.simplifide.pig

import org.apache.log4j.PropertyConfigurator
import org.apache.pig.{ExecType, PigServer}
import org.simplifide.pig.model.{PigObjects, PigModel}

import scala.collection.JavaConversions

/**
 * Created by andy on 10/12/14.
 */
object PigExec {

  PropertyConfigurator.configure("/home/andy/IdeaProjects/pig/log4j.properties")
  val server = new PigServer(ExecType.LOCAL)




  def runCommand(command:String, postfix:String) = {
    System.out.println(command)
    server.registerQuery(command + postfix)
  }


  def runModel(model:PigModel, context:PigContext) = {
    model match {
      case x:PigObjects.Store => { // Store Case - Should Be Moved to Function
        System.out.println("Storing " + x.input.create.name + " into " + x.intoModel.get)
        val name = x.input.create.name
        val result = x.usingModel match {
          case Some(y) => server.store(name ,x.intoModel.get,y)
          case _       => server.store(name,x.intoModel.get)
        }
        context.context.put(name,result)
      }
      case x:PigObjects.Dump => {

        context.get(x.input.create.name) match {
          case Some(y) => {
            val value = JavaConversions.asScalaIterator(y.getResults).mkString("\n")
            val hash = value.hashCode
            System.out.print(value)
          }
          case _       => s"Error : Can't Find ${x.input.create.name}"
        }
      }
      case x:PigObjects.ForEachApply => runCommand(PigTemplate.createTemplate(model).create,"")
      case _                         => runCommand(PigTemplate.createTemplate(model).create,";")
    }
  }

  //server.registerQuery(s"a = load '$testFile' using PigStorage() as (b:int,c:int,d:int);")
  //server.dumpSchema("a")
  //server.explain("a",System.out)

}
