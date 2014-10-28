package org.simplifide.pig.parser

import org.simplifide.pig.model.BuiltInObjects._
import org.simplifide.pig.model.StateObjects

/**
 * Created by andy on 10/26/14.
 */
trait BuiltInParser {

  type E = PigExpression

  // Standard Functions
  def avg(expr:E)                               = Avg(expr)
  def bagToString(expr:E)                       = BagToString(expr,None)
  def bagToString(expr:E, delimiter:Character)  = BagToString(expr,Some(delimiter))
  def count(expr:E)                             = Count(expr)
  def count_star(expr:E)                        = CountStar(expr)
  def diff(e1:E, e2:E)                          = Diff(e1,e2)
  def max(expr:E)                               = Max(expr)
  def min(expr:E)                               = Min(expr)
  def concat(expr:E*)                           = Concat(expr.toList)
  def isEmpty(expr:E)                           = IsEmpty(expr)
  def pluckTuple(expr:E)                        = PluckTuple(expr)
  def size(expr:E)                              = Size(expr)
  def subtract(e1:E, e2:E)                      = Subtract(e1,e2)
  def sum(expr:E)                               = Sum(expr)
  def tokenize(expr:E, delimiter:Character)     = Tokenize(expr,Some(delimiter))
  // Math Functions
  def abs (expr:E)                              = Abs(expr)
  def acos(expr:E)                              = Acos(expr)
  def asin(expr:E)                              = Asin(expr)
  def atan (expr:E)                             = Atan(expr)
  def cbrt(expr:E)                              = Cbrt(expr)
  def ceil(expr:E)                              = Ceil(expr)
  def cos(expr:E)                               = Cos(expr)
  def cosh(expr:E)                              = Cosh(expr)
  def exp(expr:E)                               = Exp(expr)
  def floor(expr:E)                             = Floor(expr)
  def log(expr:E)                               = Log(expr)
  def log10(expr:E)                             = Log10(expr)
  val random                                    = Random
  def round(expr:E)                             = Round(expr)
  def roundTo(e1:E, e2:E)                       = RoundTo(e1,e2,None)
  def roundTo(e1:E, e2:E,e3:E)                  = RoundTo(e1,e2,Some(e3))
  def sin(expr:E)                               = Sin(expr)
  def sinh(expr:E)                              = Sinh(expr)
  def sqrt(expr:E)                              = Sqrt(expr)
  def tan(expr:E)                               = Tan(expr)
  def tanh(expr:E)                              = Tanh(expr)

  // String Functions
  def endsWith(e1:E,e2:E)                      = EndsWith(e1,e2)
  def equalsIgnoreCase(e1:E,e2:E)              = EqualsIgnoreCase(e1,e2)
  def indexOf(e1:E,e2:E,e3:E)                  = IndexOf(e1,e2,e3)
  def lastIndexOf(e1:E,e2:E)                   = LastIndexOf(e1,e2)
  def cFirst(e1:E)                             = LcFirst(e1)
  def lower(e1:E)                              = Lower(e1)
  def lTrim(e1:E)                              = LTrim(e1)
  def regexExtract(e1:E,e2:E,e3:E)             = RegexExtract(e1,e2,e3)
  def regexExtractAll(e1:E,e2:E)               = RegexExtractAll(e1,e2)
  def replace(e1:E,e2:E,e3:E)                  = Replace(e1,e2,e3)
  def tTrim(e1:E)                              = RTrim(e1)
  def startsWith(e1:E)                         = StartsWith(e1)
  def strSplit(e1:E,e2:E,e3:E)                 = StrSplit(e1,e2,e3)
  def subString(e1:E,e2:E,e3:E)                = SubString(e1,e2,e3)
  def trim(e1:E)                               = Trim(e1)
  def ucFirst(e1:E)                            = UcFirst(e1)
  def upper(e1:E)                              = Upper(e1)
  // Date Functions
  def addDuration         (e:E*)              = new BaseN("addDuration",e.toList)
  def currentTime         (e:E*)              = new BaseN("currentTime",e.toList)
  def daysBetween         (e:E*)              = new BaseN("daysBetween",e.toList)
  def getDay              (e:E*)              = new BaseN("getDay",e.toList)
  def getHour             (e:E*)              = new BaseN("getHour",e.toList)
  def getMilliSecond      (e:E*)              = new BaseN("getMilliSecond",e.toList)
  def getMinute           (e:E*)              = new BaseN("getMinute",e.toList)
  def getMonth            (e:E*)              = new BaseN("getMonth",e.toList)
  def getSecond           (e:E*)              = new BaseN("getSecond",e.toList)
  def getWeek             (e:E*)              = new BaseN("getWeek",e.toList)
  def getWeekYear         (e:E*)              = new BaseN("getWeekYear",e.toList)
  def getYear             (e:E*)              = new BaseN("getYear",e.toList)
  def hoursBetween        (e:E*)              = new BaseN("hoursBetween",e.toList)
  def milliSecondsBetween (e:E*)              = new BaseN("milliSecondsBetween",e.toList)
  def minutesBetween      (e:E*)              = new BaseN("minutesBetween",e.toList)
  def monthsBetween       (e:E*)              = new BaseN("monthsBetween",e.toList)
  def secondsBetween      (e:E*)              = new BaseN("secondsBetween",e.toList)
  def subtractDuration    (e:E*)              = new BaseN("subtractDuration",e.toList)
  def toDate              (e:E*)              = new BaseN("toDate",e.toList)
  def toMilliSeconds      (e:E*)              = new BaseN("toMilliSeconds",e.toList)
  def toString            (e:E*)              = new BaseN("toString",e.toList)
  def toUnixTime          (e:E*)              = new BaseN("toUnixTime",e.toList)
  def weeksBetween        (e:E*)              = new BaseN("weeksBetween",e.toList)
  def yearsBetween        (e:E*)              = new BaseN("yearsBetween",e.toList)
  // Conversion Operations
  def toTuple             (e:E*)              = new BaseN("ToTuple",e.toList)
  def toBag               (e:E*)              = new BaseN("ToBag",e.toList)
  def toMap               (e:E*)              = new BaseN("ToMap",e.toList)
  def Top                 (e:E*)              = new BaseN("Top",e.toList)

}
