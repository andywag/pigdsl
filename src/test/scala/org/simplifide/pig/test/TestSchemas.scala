package org.simplifide.pig.test

import org.simplifide.pig.model.NewSchema



/**
 * Created by andy on 10/18/14.
 */
object TestSchemas {

  object Student extends NewSchema {
    val name  = item("name",NewSchema.String)
    val age   = item("age",NewSchema.Int)
    val gpa   = item("gpa",NewSchema.Float)
  }

  object ComplexSchema extends NewSchema {
    object t1 extends NewSchema.Tuple("t1") {
      val t1a = item("t1a",NewSchema.Int)
      val t1b = item("t1b",NewSchema.Int)
    }
    object t2 extends NewSchema.Tuple("t2") {
      val t2a = item("t2a",NewSchema.Int)
      val t2b = item("t2b",NewSchema.Int)
    }
    item("t1",t1)
    item("t2",t2)
  }

  object Integer extends NewSchema {
    val f1 = item("f1",NewSchema.Int)
    val f2 = item("f2",NewSchema.Int)
    val f3 = item("f3",NewSchema.Int)
  }

  object Voter extends NewSchema {
    val name          = item("name",NewSchema.String)
    val age           = item("age",NewSchema.Int)
    val registration  = item("registration",NewSchema.String)
    val donation      = item("donation",NewSchema.Float)
  }

}
