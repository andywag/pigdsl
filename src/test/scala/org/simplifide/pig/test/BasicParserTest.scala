package org.simplifide.pig.test

import org.scalatest.{FlatSpec, Matchers}
import org.simplifide.pig.model.NewSchema
import org.simplifide.pig.model.StateObjects.Replicated
import org.simplifide.pig.parser.BaseParser

import org.scalatest._
import org.simplifide.pig.test.TestParser.TestSchema
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

  class ComplexSchemaTest extends BasicPigTest2("ComplexSchema","") {
    import ComplexSchema._
    'a := load(baseLocation + "complex.txt") using "PigStorage(' ')" as ComplexSchema
    'b := foreach('a) generate(t1.t1a, t2.$(0))
    check('b,Some(-1592404428))
    //->(store('b) into(tempResults) using "PigStorage(' ')")
    //->(dump('b))
    'c := foreach ('a) generate flatten(ComplexSchema.t1)
    check('c,Some(1064788788))
  }

  class ExpressionTest extends BasicPigTest2("Expression","") {
    'a := load(baseLocation + "integers.txt") using "PigStorage(' ')"
    'b := foreach('a) generate ($(0) + $(2),$(1) + 5)
    check('b,Some(640593805))
    'c := foreach('a) generate(-$(0), $(1))
    val a = assert('c) by ($(0) > 0,"Data should be greater than 0")
    ->(a)
    check('c,Some(523975983))
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
    import TestSchemas.Integer._
    'a := load(baseLocation + "student.txt") using "PigStorage(' ')" as Student
    'a1 := load(baseLocation + "student.txt") using "PigStorage(' ')" as Student
    'b := foreach ('a) generate(STAR)
    check("b",Some(-1969659138))
    'c := filter ('a) by Not(Student.name === "Mary")
    check("c",Some(-2031232714))
    'd := join('a by $(0)-->$(1), 'a1 by $(0)-->$(1))
    check("d",Some(995180772))
    'e := load(baseLocation + "integers.txt") as TestSchemas.Integer
    val c = Case(f2 % 2) when (0) then ("even") when(1) then ("odd")
    'f := foreach('e) generate ( c)
    check('f,Some(-949653407) )

    val d = Case when (f2 % 2 === 0) then ("even") when(f2 % 2 === 1) then ("odd")
    'g := foreach('e) generate (f2, d)
    check('g,Some(-1619898405) )

    'h := filter('a) by(Student.name matches ".*a.*")
    check('h,Some(1651048466))

    'i := foreach('a) generate Student.name


  }

  class CastTest extends BasicPigTest2("Expression","") {
    import TestSchemas.Integer._

    'a := load(baseLocation + "integers.txt") using "PigStorage(' ')" as TestSchemas.Integer
    'b := group('a all)
    'c := foreach('b) generate (sum('a~>f1)) as('total)
    'd := foreach('a) generate f1/double('c~>'total)
     check('d,Some(-1791926720))

    'e := foreach ('b) generate (sum('a~>f1) as ('total), count('a) as 'cnt);
    'f := filter ('a) by (f1 > 0)
    'g := foreach ('f) generate (f1,f2)
    check('g,None)

  }

class MiscTest extends BasicPigTest2("Expression","") {
  import TestSchemas.Integer._

  'a := load(baseLocation + "integers.txt") using "PigStorage(' ')" as TestSchemas.Integer
  'b := load(baseLocation + "integers.txt") using "PigStorage(' ')" as TestSchemas.Integer
  'c := cross('a,'b)
  check('c,Some(1427312791))
  'd := cube('a) by (cubeI(f1,f2), rollup(f3))
  check('d,Some(-318962588))
  'e := filter('a) by((f1 === 4) And Not (f2 === 3))
  check('e,Some(-1623584726))
  'f := foreach('a) generate STAR
  check('f,Some(252234730))

}

class NestedTest extends BasicPigTest2("Nested","") {
  import TestSchemas.Student._
  'a1 := load(baseLocation + "student.txt") using "PigStorage(' ')" as Student
  'a2 := load(baseLocation + "student.txt") using "PigStorage(' ')" as Student

  'b  := cogroup('a1 by age, 'a2 by age)
  'c  := foreach ('b) (
    'd ::= cross('a1, 'a2),
     generate ('d)
  )
  check('c,Some(-863636984))
  'e  := group('a1 by age)
  'f  := foreach ('e) (
    'd ::= foreach ('a1) generate age,
     generate ('d)
  )
  check('f,Some(-2017674200))

  'g := join('a1 by age, 'a2 by age)
  check('g,Some(865146468))

  'h := join ('a1 by $(0) Left, 'a2 by $(0)) using Replicated
  check('h,Some(865146468))

  'i := union ('a1, 'a2)
  check ('i,Some(-673919566))


}

class OtherTest extends BasicPigTest2("Nested","") {

  import TestSchemas.Integer._

  'a := load(baseLocation + "integers.txt") using "PigStorage(' ')" as TestSchemas.Integer
  'b := group('a all)
  'c := foreach('b) generate(count('a)) as 'sum
  'd := order ('a) by ($(0) Desc)
  'e := limit('d,'c~>'sum - 1)
  check('e,Some(-1493911711))

  // TODO : Fix the Requirements for Parens on Dense
  'f := (rank('a) by ($(0) Desc, $(1) Asc)).dense
  check('f,Some(540017304))
  //'g := sample('f, .5)
  //check('g,Some(-2067934372))

  ->(split('a) into ('x iff (f1 < 2), 'y Otherwise))
  check('x,Some(-1709561645))
  check('y,Some(311033441))

}

class StreamTest extends BasicPigTest2("Nested","") {

  import TestSchemas.Integer._

  // TODO : Need Support for Stream Test

}


  // TODO : Need to Support Map Schema -- With Type








