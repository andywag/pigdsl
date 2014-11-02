package org.simplifide.pig.user

/**
 * Created by andy on 10/31/14.
 */

import org.apache.pig.impl.logicalLayer.schema.Schema
import org.apache.pig.impl.logicalLayer.schema.Schema.FieldSchema
import org.apache.pig.{FuncSpec, EvalFunc}
import org.apache.pig.data.{DataType, Tuple}

import scala.reflect.runtime.{universe=>ru}

/*

class UserReflect[T](implicit val typ:ru.TypeTag[T]) {


  def createMethod(method:ru.MethodSymbol) = {
    def ports(ports:List[List[ru.Symbol]]) = {
      def port(port:ru.TermSymbol) = new UserReflect.Port(port.name.toString,port.typeSignature)
      ports(0).map(x => port(x.asTerm))
    }
    new UserReflect.Method(method.name.toString,ports(method.paramss))
  }

  val declarations = ru.typeOf[T].declarations
  val methods = declarations.filter(x => x.isMethod).map(x => x.asMethod)
  val realMethods = methods.map(x => createMethod(x))





}

class UserEvalBase extends EvalFunc[Any] {

  val items:Map[String,UserEvalFunc[_,_]]
  def exec
}

class UserEvalFunc[T,S](f:(S)=>T) extends EvalFunc[Any] {
  def exec(input:Tuple): Any = {
    if (input == null || input.size() == 0 ) ""
    else input.get(0).toString.toUpperCase
    /*
    val value = input.get(0).asInstanceOf[S]
    f(value)
    */
  }
  def create = {
    val schema = new Schema(new FieldSchema(null,DataType.UNKNOWN))
    new FuncSpec("org.simplifide.pig.user.UserEvalFunc",schema)
  }
}

object UserReflect {



  class Method(val name:String, val ports:List[Port]) {
    def createFunc = {

    }
  }
  class Port(val name:String, val typ:ru.Type)

  def main(args:Array[String]) = {
    val str = new UserReflect[String]()
    val meth = str.realMethods

    val f = (_:String).toUpperCase
    new UserEvalFunc(f)

    System.out.println("Here" + meth)


  }

}
*/