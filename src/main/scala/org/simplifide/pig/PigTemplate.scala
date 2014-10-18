package org.simplifide.pig

/**
 * Created by andy on 10/11/14.
 */

import org.simplifide.pig.model.{NewSchema, PigObjects}
import org.simplifide.pig.parser.PigExpression
import org.simplifide.template.Template
import Template._

object PigTemplate {

  def commaList(input:List[Any]) = sep(input.map(C(_)),",")

  def createTemplate(model:Any):Template = {
    model match {
      case   PigObjects.NULL        => Template.StringToTemplate("null")
      case x:PigExpression.QuestionGroup => C(x.lhs) ~ " ? " ~ C(x.rhs)
      case x:PigExpression.QuestionOpen  => C(x.lhs) ~ " : " ~ C(x.tr)
      case x:PigExpression.QuestionClose => C(x.lhs) ~ " ? " ~ C(x.tr) ~ " : " ~ C(x.fa)
      case x:PigObjects.Flatten     => "flatten " ~ paren(C(x.expr))
      case x:PigObjects.IsEmpty     => "IsEmpty"  ~ paren(C(x.expr))
      case x:PigObjects.Dollar      => Template.StringToTemplate("$" + s"${x.value}")
      case x:PigObjects.Assign      => C(x.lhs) ~ " = " ~ C(x.rhs)
      case x:PigObjects.Loader      => loader(x)
      case x:PigObjects.OrderBy     => orderBy(x)
      case x:NewSchema.Pair         => Template.StringToTemplate(x.complexName.toString)
      case x:NewSchema.TupleDollar  => Template.StringToTemplate(x.complexName.toString)
      case x:NewSchema              => schema(x)
      case x:PigObjects.PigSymbol   => x.symbol.toString().substring(1)
      case x:PigObjects.PigInt      => x.value.toString
      case x:PigObjects.Cross       => cross(x)
      case x:PigObjects.CubeBy      => cube(x)
      case x:PigObjects.CubeInner   => cubeI("CUBE",x)
      case x:PigObjects.RollUp      => cubeI("ROLLUP",x)
      case x:PigExpression.Binary   => binaryop(x)
      case x:PigObjects.Distinct    => distinct(x)
      case x:PigObjects.FilterBy    => filter(x)
      case x:PigObjects.GroupBy     =>
        "GROUP " ~ commaList(x.inputs) ~ pre("USING",x.using) ~ partitionBy(x.partitionBy) ~ par(x.par)
      case   PigObjects.Collected   => Template.StringToTemplate("'collected'")
      case   PigObjects.Merge       => Template.StringToTemplate("'merge'")
      case x:PigObjects.Import      => "IMPORT" ~ surround(x.input,"\"")
      case x:PigObjects.Join        => join(x)
      case x:PigObjects.JoinBy      => joinBy(x)
      case x:PigObjects.JoinUsing   => Template.StringToTemplate(x.value)
      case x:PigObjects.SymbolBy    => C(x.lhs) ~ " by " ~ C(x.rhs)
      case x:PigObjects.SymbolByDirection => C(x.symbol) ~ Template.StringToTemplate(x.text)
      case x:PigObjects.Limit        => "LIMIT " ~ C(x.input) ~ " " ~ C(x.limit)
      case x:PigObjects.Sample       => "SAMPLE " ~ C(x.input) ~ " " ~ C(x.limit)
      case x:PigObjects.IfExpression => C(x.lhs) ~ " IF " ~ C(x.rhs)
      case x:PigObjects.Rank        =>
        "RANK " ~ C(x.input) ~ opt(" by " ~ sep(x.by.map(C(_)),","),x.by.size > 0)  ~ opt(" dense ",x.denseO)
      case x:PigObjects.SplitInto   =>
        "SPLIT " ~ C(x.input) ~ " INTO "  ~ sep(x.expressions.map(C(_)),",")
      case x:PigObjects.StreamThrough =>
        "STREAM " ~ sep(x.stream.inputs.map(C(_)),",") ~ " THROUGH " ~ C(x.through) ~ as(x.schema)

      case x:PigObjects.MapReduceStoreUsing =>
        "MAPREDUCE " ~ C(x.jar) ~ " STORE " ~ C(x.store) ~ " INTO " ~ C(x.into) ~ " USING " ~ C(x.usingStore)
      case x:PigObjects.MapReduceLoadUsing =>
        C(x.store) ~ " LOAD " ~ C(x.location) ~ " USING " ~ C(x.using)
      case x:PigObjects.MapReduceSchema =>
        C(x.using) ~ as(Some(x.schema))

      case x:PigObjects.ForEachGenerate =>
        "FOREACH " ~ C(x.input) ~ " GENERATE " ~ sep(x.expr.map(C(_)),",") ~ as(x.as)

      case x:PigObjects.ForEachApply =>
        "FOREACH " ~ C(x.input) ~ " {\n" ~ sep(x.inputs.map(C(_)),"\n;") ~ "}\n"

      case x:String                 => Template.StringToTemplate(x)
      case x:Template               => x
      case _                        => {System.out.println("Error " + model); Template.StringToTemplate("Fail")}

    }
  }

  def createSchema(model:Any):Template = {
    model match {
      case x:NewSchema.Pair  => x.name ~ ":" ~ schemaType(x.typ)
      case x:NewSchema.TupleTrait => "tuple" ~ parenComma(x.newItems.toList.map(createSchema(_)))
      case _                 => createTemplate(model)
    }
  }

  def schemaItems(model:PigExpression):Template = {
    model match {
      case x:NewSchema.Pair => x.name ~ ":" ~ schemaType(x.typ)
      case _                => createTemplate(model)
    }
  }

  def C(model:Any):Template = createTemplate(model)

  def pre(value:String, input:Option[Any]) = {
    opt(value ~ opt(input.map(C(_))),input.isDefined)
  }

  def par(input:Option[PigExpression])        = pre(" parallel ",input)
  def partitionBy(input:Option[String])       = pre(" partition by ", input)
  def use(usingModel:Option[String])          = pre(" using ", usingModel)
  def as (schema:Option[Any])                 = pre(" as " , schema)


  def schemaType(typ:NewSchema.Type) = {
    typ match {
      case   NewSchema.Int     => Template.StringToTemplate("int")
      case   NewSchema.String  => Template.StringToTemplate("chararray")
      case   NewSchema.Float   => Template.StringToTemplate("float")
      case x:NewSchema.TupleTrait   => createSchema(x)
      case _                   => Template.StringToTemplate("error")
    }
  }


  def orderBy(orderBy:PigObjects.OrderBy) = {
    def by(input:PigExpression) = new Template.StringValue(input.create.name)
    "order " ~ C(orderBy.input) ~ " by " ~ sep(orderBy.inputs.map(by(_)),",") ~ par(orderBy.par)
  }



  def schema(schema:NewSchema) = {
    paren(commaSep(schema.newItems.map(createSchema(_)).toList))
  }



  def loader(model:PigObjects.Loader) =
    "LOAD " ~ surround(model.ident,"'","' ") ~ use(model.usingModel) ~ as(model.schema)


  def store(model:PigObjects.Store) = {
    "STORE " ~ C(model.input) ~ "INTO " ~ surround(model.intoModel.get,"'","' ") ~ use(model.usingModel)
  }

  def cross(model:PigObjects.Cross) = {
    "CROSS " ~ sep(model.inputs.map(C(_)),",") ~ partitionBy(model.partitionBy) ~ par(model.par)
  }

  def binaryop(model:PigExpression.Binary) = {
    C(model.lhs) ~ " " ~ model.op ~ " " ~ C(model.rhs)
  }

  def cube(model:PigObjects.CubeBy) = {
    "CUBE " ~ C(model.input) ~ " by " ~ sep(model.inputs.map(C(_)),",") ~ par(model.par)
  }

  def cubeI(name:String,model:PigObjects.CubeInput) = {
    val temp = Template.L(model.input.map(C(_)))
    name ~ " " ~ surround(sep(temp,","),"(",")")
  }

  def distinct(model:PigObjects.Distinct) = {
    "DISTINCT " ~ C(model.input) ~ partitionBy(model.partitionBy) ~ par(model.par)
  }

  def filter(model:PigObjects.FilterBy) = {
    "FILTER " ~ C(model.input) ~ " by " ~ C(model.byTerm)
  }

  def group(model:PigObjects.GroupBy) = {

    "GROUP " ~ sep(model.inputs.map(C(_)),",") ~ pre("using",model.using) ~ partitionBy(model.partitionBy) ~ par(model.par)
  }

  def join(model:PigObjects.Join)     = "JOIN " ~ sep(model.expressions.map(C(_)),",")
  def joinBy(model:PigObjects.JoinBy) = {
    join(model.join) ~ pre("using",model.using) ~ partitionBy(model.partitionBy) ~ par(model.par)
  }


}
