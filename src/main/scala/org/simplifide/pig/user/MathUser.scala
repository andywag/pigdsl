package org.simplifide.pig.user

/**
 * Created by andy on 10/30/14.
 */

import scala.reflect.runtime.{universe=>ru}

object MathUser {
  val m       = ru.runtimeMirror(getClass.getClassLoader)
  val math    = ru.typeOf[Math].members


  def main(args:Array[String]): Unit = {
    System.out.println("M" + math)
  }


}
