package org.simplifide.pig.test

import org.scalatest.{FlatSpec, Matchers}
import org.simplifide.pig.model.NewSchema
import org.simplifide.pig.parser.BaseParser

import org.scalatest._
import org.simplifide.pig.test.TestSchemas.ComplexSchema.{t2, t1}
import org.simplifide.pig.test.TestSchemas.{Voter, ComplexSchema, Student}

import scala.reflect.io.Path

/**
 * Created by andy on 10/17/14.
 */

  import TestConstants._

  class BasicParserTest extends BasicPigTest("Basic","",Some(1641423280)) {
    import Student._
    'a := load(baseLocation + "student.txt") using "PigStorage(' ')" as Student
    'b := foreach('a) generate (Student.name,$(2))
    ->(store('b) into(tempResults) using "PigStorage(' ')")
    ->(dump('b))
  }

  class ComplexSchemaTest extends BasicPigTest("ComplexSchema","",Some(-1592404428)) {
    import ComplexSchema._
    'a := load(baseLocation + "complex.txt") using "PigStorage(' ')" as ComplexSchema
    'b := foreach('a) generate(t1.t1a, t2.$(0))
    ->(store('b) into(tempResults) using "PigStorage(' ')")
    ->(dump('b))
  }

  class ExpressionTest extends BasicPigTest("Expression","",Some(640593805)) {
    'a := load(baseLocation + "integers.txt") using "PigStorage(' ')"
    'b := foreach('a) generate ($(0) + $(2),$(1) + 5)
    ->(store('b) into(tempResults) using "PigStorage(' ')")
    ->(dump('b))
  }

  class GroupTest extends BasicPigTest("Group","",Some(1953912989)) {
    import TestSchemas.Integer._
    'a := load(baseLocation + "integers.txt") using "PigStorage(' ')" as TestSchemas.Integer
    'b := group('a by f1)
    ->(store('b) into(tempResults) using "PigStorage(' ')")
    ->(dump('b))
  }

  class NullTest extends BasicPigTest("Group","",Some(1953912989)) {
    import TestSchemas.Student._
    import TestSchemas.Voter._
    'a := load(baseLocation + "student.txt") using "PigStorage(' ')" as Student
    'v := load(baseLocation + "voter.txt") using "PigStorage(' ')" as Student
    'b := cogroup('a by Student.name, 'v by Voter.name)
    'c := foreach ('b) generate (flatten( isEmpty('a) ?? NULL :: 'a), flatten(isEmpty('b) ?? NULL :: 'b ))
    ->(store('b) into(tempResults) using "PigStorage(' ')")
    ->(dump('b))
  }




