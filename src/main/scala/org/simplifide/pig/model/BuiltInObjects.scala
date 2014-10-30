package org.simplifide.pig.model

import org.simplifide.pig.parser.PigExpression

/**
 * Created by andy on 10/26/14.
 */
object BuiltInObjects {

  type E = PigExpression

  trait BaseTrait extends PigModel{
    val fName:String
    val expressions:List[E]
  }

  class Base(val fName:String, val expr:PigExpression) extends BaseTrait {
    val expressions = List(expr)
  }
  class Base2(val fName:String, val e1:E, val e2:E) extends BaseTrait {
    val expressions = List(e1,e2)
  }
  class Base3(val fName:String, val e1:E, val e2:E, val e3:E) extends BaseTrait {
    val expressions = List(e1,e2,e3)
  }

  class BaseN(val fName:String, val expressions:List[E]) extends PigModel

  // Standard Functions
  case class Avg(override val expr:PigExpression) extends Base("AVG",expr)
  // TODO : Need Template Addition -- Testing
  case class BagToString(val expr:PigExpression, delimiter:Option[Char]) extends PigModel
  case class Concat(val expressions:List[PigExpression])                 extends PigModel
  case class IsEmpty(override val expr:PigExpression)                    extends Base("IsEmpty",expr)
  case class Count(override val expr:PigExpression)                      extends Base("COUNT",expr)
  case class CountStar(override val expr:PigExpression)                  extends Base("COUNT_STAR",expr)
  case class Diff(e1:PigExpression, e2:PigExpression)                    extends PigModel
  case class Max(override val expr:PigExpression)                        extends Base("MAX",expr)
  case class Min(override val expr:PigExpression)                        extends Base("Min",expr)
  case class PluckTuple(override val expr:PigExpression)                 extends Base("PluckTuple",expr)
  case class Size(override val expr:PigExpression)                       extends Base("Size",expr)
  case class Subtract(e1:PigExpression, e2:PigExpression)                extends PigModel
  case class Tokenize(val expr:PigExpression, delimiter:Option[Char])    extends PigModel
  case class Sum(override val expr:PigExpression)                        extends Base("SUM",expr)
  // Match Functions
  case class Abs    (override val expr:PigExpression)                    extends Base("ABS",expr)
  case class Acos   (override val expr:PigExpression)                    extends Base("ACOS",expr)
  case class Asin   (override val expr:PigExpression)                    extends Base("ASIN",expr)
  case class Atan   (override val expr:PigExpression)                    extends Base("ATAN",expr)
  case class Cbrt   (override val expr:PigExpression)                    extends Base("CBRT",expr)
  case class Ceil   (override val expr:PigExpression)                    extends Base("CEIL",expr)
  case class Cos    (override val expr:PigExpression)                    extends Base("COS",expr)
  case class Cosh   (override val expr:PigExpression)                    extends Base("COSH",expr)
  case class Exp    (override val expr:PigExpression)                    extends Base("EXP",expr)
  case class Floor  (override val expr:PigExpression)                    extends Base("FLOOR",expr)
  case class Log    (override val expr:PigExpression)                    extends Base("LOG",expr)
  case class Log10  (override val expr:PigExpression)                    extends Base("LOG10",expr)
  case object Random                                                     extends PigModel
  case class Round  (override val expr:PigExpression)                    extends Base("ROUND",expr)
  case class RoundTo(e1:PigExpression,e2:PigExpression,e3:Option[PigExpression]) extends PigModel
  case class Sin    (override val expr:PigExpression)                    extends Base("SIN",expr)
  case class Sinh   (override val expr:PigExpression)                    extends Base("SINH",expr)
  case class Sqrt   (override val expr:PigExpression)                    extends Base("SQRT",expr)
  case class Tan    (override val expr:PigExpression)                    extends Base("TAN",expr)
  case class Tanh   (override val expr:PigExpression)                    extends Base("TANH",expr)

  // String Objects

  case class EndsWith(a1:E,a2:E)                                         extends Base2("EndsWith",a1,a2)
  case class EqualsIgnoreCase(a1:E,a2:E)                                 extends Base2("EqualsIgnoreCase",a1,a2)
  case class IndexOf(a1:E,a2:E,a3:E)                                     extends Base3("IndexOf",a1,a2,a3)
  case class LastIndexOf(a1:E,a2:E)                                      extends Base2("LastIndexOf",a1,a2)
  case class LcFirst(a1:E)                                               extends Base("LcFirst",a1)
  case class Lower(a1:E)                                                 extends Base("Lower",a1)
  case class LTrim(a1:E)                                                 extends Base("LTrim",a1)
  case class RegexExtract(a1:E,a2:E,a3:E)                                extends Base3("RegexExtract",a1,a2,a3)
  case class RegexExtractAll(a1:E,a2:E)                                  extends Base2("RegexExtractAll",a1,a2)
  case class Replace(a1:E,a2:E,a3:E)                                     extends Base3("Replace",a1,a2,a3)
  case class RTrim(a1:E)                                                 extends Base("RTrim",a1)
  case class StartsWith(a1:E)                                            extends Base("StartsWith",a1)
  case class StrSplit(a1:E,a2:E,a3:E)                                    extends Base3("StrSplit",a1,a2,a3)
  case class SubString(a1:E,a2:E,a3:E)                                   extends Base3("SubString",a1,a2,a3)
  case class Trim(a1:E)                                                  extends Base("Trim",a1)
  case class UcFirst(a1:E)                                               extends Base("UcFirst",a1)
  case class Upper(a1:E)                                                 extends Base("Upper",a1)

  // Date Objects



}
