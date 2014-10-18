package org.simplifide.template

/**
 * Created by andy.wagner on 9/26/2014.
 */

class GenericTemplateConstructor {

}

object GenericTemplateConstructor {

  trait T {
    def flatten(in:T)
  }

  case class Constructor1[A <: T,B](val in:B, parent:Option[A]) extends T {
    def flatten(in:T) = parent.map(x => x.flatten(this))
  }

  case class Constructor2[A <: T,B,C](val in:B, parent:Option[A]) extends T {
    def -> (in:C):Constructor1[Constructor2[A,B,C],C] = Constructor1(in,Some(this))
    def flatten(in:T) = parent.map(x => x.flatten(this))
  }

  case class Constructor3[A <: T,B,C](val parent:Option[A]) extends T  {
    def ->(in:B):Constructor2[Constructor3[A,B,C],B,C] = Constructor2(in,Some(this))
    def flatten(in:T) = parent.map(x => x.flatten(this))

  }

  //case class Holder1[A]
  case class Holder2[A,B](val a:A, val b:B)
  //case class Holder3[A,B,C]


  def main(args:Array[String]) = {
    val alpha = Constructor3[T,String,String](None)
    val d = alpha ->("a") ->("b")
    System.out.println(d.getClass + d.flatten(null).toString)
  }



  //class Constructor[T,S](obj:GenericTemplateConstructor[T,S],input:T) {
  //  def -> (input:T) = GenericTemplateConstructor(this,new And(this.input,input))
    //def ->(template:Template) = new TemplateConstructor.Constructor(this.instance,this.template ~ template)
  //}

  //class And[T,S](val t:T,val s:S)

  //class Alpha[T,S] extends GenericTemplateConstructor[And[Int,String]] {

  //}

}

