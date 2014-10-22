package org.simplifide.pig.test

import org.scalatest.{FlatSpec, Matchers}
import org.simplifide.pig.model.NewSchema
import org.simplifide.pig.parser.BaseParser

import org.scalatest._
import org.simplifide.pig.test.TestSchemas.ComplexSchema.{t2, t1}
import org.simplifide.pig.test.TestSchemas.{StudentHolder, Voter, ComplexSchema, Student}

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

  class NullTest extends BasicPigTest("Group","",Some(865146468)) {
    import TestSchemas.Student._
    'a1 := load(baseLocation + "student.txt") using "PigStorage(' ')" as Student
    'a2 := load(baseLocation + "student.txt") using "PigStorage(' ')" as Student

    'c1 := filter ('a1) by (Student.name isNotNull)
    'c2 := filter ('a2) by (Student.name isNotNull)
    'b := join('c1 by age, 'c2 by age)
    ->(store('b) into(tempResults) using "PigStorage(' ')")
    ->(dump('b))
  }

  class FilterTest extends BasicPigTest("Filter","",Some(1129032221)) {
    import TestSchemas.StudentHolder._
    'a := load(baseLocation + "studentT.txt") using "PigStorage(' ')" as StudentHolder
    'c := filter ('a) by S === T("John",18,4.0)
    'b := foreach ('c) generate (S.name1, M(("A","B")),T(1,2,3), B(T(1,2), T(2,3)))

    ->(store('b) into(tempResults) using "PigStorage(' ')")
    ->(dump('b))
  }

  class ExpressionTest2 extends BasicPigTest2("Expression","") {
    import TestSchemas.Student._
    'a := load(baseLocation + "student.txt") using "PigStorage(' ')" as Student
    'a1 := load(baseLocation + "student.txt") using "PigStorage(' ')" as Student
    'b := foreach ('a) generate(STAR)
    check("b",Some(-1969659138))
    'c := filter ('a) by not(Student.name === "Mary")
    check("c",Some(-2031232714))
    'd := join('a by $(0)-->$(1), 'a1 by $(0)-->$(1))
    check("d",Some(995180772))

    'e := foreach ('a) generate (Case(age) when (18) then ("here") when (20) then ("there") Else ("nowhere")  )
  }

  // TODO : Need to Support Map Schema -- With Type








