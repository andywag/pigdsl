package org.simplifide.pig.core

import org.apache.pig.backend.executionengine.ExecJob

import scala.collection.{JavaConversions, mutable}

/**
 * Created by andy on 10/14/14.
 */
class PigRunContext {

  val context = new mutable.HashMap[String,ExecJob]()
  def get(name:String) = context.get(name)

  def results(name:String) = context.get(name).map(x => JavaConversions.asScalaIterator(x.getResults).mkString("\n"))

}


