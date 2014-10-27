package org.simplifide.pig.parser

import org.simplifide.pig.model.StateObjects

/**
 * Created by andy on 10/25/14.
 */
trait ConstantParser {


  // Constants
  val NULL                               = StateObjects.NULL

  val STAR                               = DirectTemplateParser.T("*")
  val GROUP                              = DirectTemplateParser.T("group")
  val AT                                 = DirectTemplateParser.T("@")
  val EMPTY                              = DirectTemplateParser.T("")

  // Casting Operators
  def bag(expr:PigExpression)            = DirectTemplateParser.Cast(expr,"bag")
  def tuple(expr:PigExpression)          = DirectTemplateParser.Cast(expr,"tuple")
  def int(expr:PigExpression)            = DirectTemplateParser.Cast(expr,"int")
  def long(expr:PigExpression)           = DirectTemplateParser.Cast(expr,"long")
  def float(expr:PigExpression)          = DirectTemplateParser.Cast(expr,"float")
  def double(expr:PigExpression)         = DirectTemplateParser.Cast(expr,"double")
  def chararray(expr:PigExpression)      = DirectTemplateParser.Cast(expr,"chararray")
  def bytearray(expr:PigExpression)      = DirectTemplateParser.Cast(expr,"bytearray")
  def boolean(expr:PigExpression)        = DirectTemplateParser.Cast(expr,"boolean")

}


