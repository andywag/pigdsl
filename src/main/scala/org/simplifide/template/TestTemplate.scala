package org.simplifide.template

/**
 * Created by andy.wagner on 9/18/2014.
 */

import Template._

object TestTemplate {



  object TestFile extends ScalaTemplateBuilder.ScalaFile {
    val pack = new StringValue("org.simplifide.parser")

    $import("org.simplifide.parser.alpha")
    $import("org.simplifide.parser.beta")

    items(Obj1)

    object Obj1 extends ScalaTemplateBuilder.ScalaObject {
      val name = "Obj1"

      items(ScalaTemplate.$val("alpha","beta"))
      items(Tra1)

      object Tra1 extends ScalaTemplateBuilder.ScalaTrait {
        val name = "Tra1"
        items(ScalaTemplate.$val("alpha","beta"))

      }

    }

  }

  def main(args:Array[String]) = {
    //val file = new ScalaTemplateBuilder.ScalaFile()
    //System.out.println(file.construct)
    System.out.println(TestFile.create)
  }

}
