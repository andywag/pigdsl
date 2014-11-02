package org.simplifide.pig.parser

import org.simplifide.pig.user.UserDefined

import scala.collection.mutable.ListBuffer

/**
 * Created by andy on 11/2/14.
 */
trait UserDefinedParser {

  val functions = new ListBuffer[UserDefined]()

  def registerFunction(userDefined:UserDefined) = {
    functions.append(userDefined)
    userDefined
  }



}
