package org.simplifide.pig

/**
 * Created by andy on 10/11/14.
 */

import org.simplifide.pig.model.ExpressionObjects.PigAll
import org.simplifide.pig.model.{ExpressionObjects => EO, _}
import ModelBase.{DoubleModel, StringModel}
import org.simplifide.pig.parser.{DirectTemplateParser, PigExpression}
import org.simplifide.pig.user.UserDefined
import org.simplifide.template.Template
import Template._
import org.simplifide.pig.model.{StateObjects => PO}

object PigTemplate {

  def commaList(input:List[Any]) = sep(input.map(C(_)),",")

  def createTemplate(model:Any):Template = {
    model match {
      // User Defined Methods
      case x:UserDefined.UserDefinedFunction => x.functionName ~ paren(commaList(x.expressions))

      case x:StateObjects.Direct       => x.value
      case x:DirectTemplateParser.CaseClose => "CASE " ~ C(x.expr) ~ sep(x.clauses.map(C(_))," ") ~" END"
      case x:DirectTemplateParser.TemplateModel       => x.template
      // Basic Types
      case x:StringModel               => surround(x.name,"'","'")
      case x:DoubleModel               => x.value.toString ~ "F"
      case x:ModelBase.CharModel     => "'" ~ x.value.toString ~ "'"
      case x:ModelBase.IntModel      => x.value.toString
      // Constants
      case   StateObjects.NULL            => Template.StringToTemplate(" NULL ")
      // Unary Expressions
      case x:EO.Negate                  => C("-") ~ C(x.lhs)
      case x:EO.Not                     => "NOT " ~ C(x.lhs)
      // PostFix Operations
      case x:PigAll                     => C(x.expr) ~ " ALL"
      // Special Unary Expressions
      case x:EO.Left         => C(x.expr) ~ " LEFT "  ~ opt(" OUTER ",x.outer)
      case x:EO.Right        => C(x.expr) ~ " RIGHT " ~ opt(" OUTER ",x.outer)
      case x:EO.Full         => C(x.expr) ~ " FULL "  ~ opt(" OUTER ",x.outer)
      case x:EO.Asc          => C(x.expr) ~ " ASC "
      case x:EO.Desc         => C(x.expr) ~ " DESC "
      // TODO : Move Othewise somewhere else
      case x:EO.IfExpression => C(x.lhs) ~ " IF " ~ C(x.rhs)
      case x:EO.Otherwise    => C(x.expr) ~ " OTHERWISE "
      // Binary Expressions
      case x:EO.Binary                  => paren(C(x.lhs) ~ " " ~ x.op ~ " " ~ C(x.rhs))
      case x:EO.BinaryNoSpace           => paren(C(x.lhs) ~ x.op ~ C(x.rhs))
      case x:EO.Arrow                   => C(x.lhs) ~ "#" ~ C(x.rhs)
      case x:EO.IsNull                  => C(x.lhs) ~ " IS NULL "
      case x:EO.IsNotNull               => C(x.lhs) ~ " IS NOT NULL "
      case x:EO.QuestionGroup           => C(x.lhs) ~ " ? " ~ C(x.rhs)
      case x:EO.QuestionOpen            => C(x.lhs) ~ " : " ~ C(x.tr)
      case x:EO.QuestionClose           => C(x.lhs) ~ " ? " ~ C(x.tr) ~ " : " ~ C(x.fa)
      case x:EO.As                      => C(x.lhs) ~ " AS " ~ C(x.input)
      case x:EO.Match                   => C(x.lhs) ~ " MATCHES " ~ C(x.input)
      case x:EO.And                     => C(x.lhs) ~ " AND " ~ C(x.rhs)
      case x:EO.Or                      => C(x.lhs) ~ " OR " ~ C(x.rhs)
      // Schema Objects
      case x:SchemaObjects.Tuple        => paren(commaList(x.expressions))
      case x:SchemaObjects.Bag          => curly(commaList(x.expressions))
      case x:SchemaObjects.MapPig       => brack(commaList(x.expressions))
      // TODO : Needs Cleanup of these Expressions
      case x:NewSchema.Pair             => Template.StringToTemplate(x.complexName.toString)
      case x:NewSchema.TupleDollar      => Template.StringToTemplate(x.complexName.toString)
      case x:NewSchema.Tuple            => C(x.complexName)
      case x:NewSchema                  => schema(x)
      // Operations
      case x:PO.AssertBy                => "ASSERT " ~ C(x.lhs) ~ " BY " ~ C(x.rhs) ~ "," ~ opt(x.message.map(x => C(StringModel(x))))
      case x:PO.Cross                   => "CROSS " ~ sep(x.inputs.map(C(_)),",") ~ partitionBy(x.partitionBy) ~ par(x.par)
      case x:PO.CubeBy                  => "CUBE " ~ C(x.input) ~ " by " ~ sep(x.inputs.map(C(_)),",") ~ par(x.par)
        case x:PO.CubeInner             => cubeI("CUBE",x)
        case x:PO.RollUp                => cubeI("ROLLUP",x)
      case x:PO.Distinct                => "DISTINCT " ~ C(x.input) ~ partitionBy(x.partitionBy) ~ par(x.par)
      case x:PO.FilterBy                => "FILTER " ~ C(x.input) ~ " by " ~ C(x.byTerm)
      case x:PO.ForEachGenerate         => "FOREACH " ~ C(x.input) ~ " GENERATE " ~ commaList(x.expr) ~ as(x.as)
      case x:PO.ForEachApply            => "FOREACH " ~ C(x.input) ~ " {\n" ~ sep(x.inputs.map(C(_)),";\n  ") ~ ";\n}\n"
      case x:PO.Generate                => "GENERATE " ~ C(x.expr)
      case x:PO.GroupBy                 => "GROUP " ~ commaList(x.inputs) ~ pre("USING",x.using) ~ partitionBy(x.partitionBy) ~ par(x.par)
      case x:PO.Join                    => "JOIN " ~ commaList(x.expressions)
      case x:PO.JoinBy                  =>  join(x.join) ~ pre("USING",x.using) ~ partitionBy(x.partitionBy) ~ par(x.par)
        case x:PO.JoinUsing             =>  Template.StringToTemplate(x.value)
      case x:PO.Limit                   => "LIMIT " ~ C(x.input) ~ " " ~ C(x.limit)
      case x:PO.Loader                  => "LOAD " ~ surround(x.ident,"'","' ") ~ use(x.usingModel) ~ as(x.schema)
      case x:PO.MapReduceStoreUsing     => "MAPREDUCE " ~ C(x.jar) ~ " STORE " ~ C(x.store) ~ " INTO " ~ C(x.into) ~ " USING " ~ C(x.usingStore)
      case x:PO.MapReduceLoadUsing      => C(x.store) ~ " LOAD " ~ C(x.location) ~ " USING " ~ C(x.using)
      case x:PO.MapReduceSchema         => C(x.using) ~ as(Some(x.schema))
      case x:PO.OrderBy                 => "ORDER " ~ C(x.input) ~ " by " ~ commaList(x.inputs) ~ par(x.par)
      case x:PO.Rank                    => "RANK " ~ C(x.input) ~ opt(" BY " ~ sep(x.by.map(C(_)),","),x.by.size > 0)  ~ opt(" dense ",x.denseO)
      case x:PO.Sample                  => "SAMPLE " ~ C(x.input) ~ " " ~ C(x.limit)
      case x:PO.SplitInto               => "SPLIT " ~ C(x.input) ~ " INTO "  ~ sep(x.expressions.map(C(_)),",")
      case x:PO.StreamThrough           => "STREAM " ~ sep(x.stream.inputs.map(C(_)),",") ~ " THROUGH " ~ C(x.through) ~ as(x.schema)
      case x:PO.Union                   => " UNION " ~ opt( " ONSCHEMA ",x.onSchema) ~ commaList(x.expressions)
      case x:PO.RegisterJar             => "REGISTER " ~ C(x.value)
      // BuiltIn Objects
      case x:BuiltInObjects.BaseTrait   => x.fName ~ paren(commaList(x.expressions))

      case x:BuiltInObjects.BagToString => "BagToString " ~ paren(C(x.expr) ~ opt(x.delimiter.map(y => "," ~ C(y))))
      case x:BuiltInObjects.Concat      => "CONCAT " ~ paren(commaList(x.expressions))
      case x:BuiltInObjects.Diff        => "DIFF " ~ paren(C(x.e1) ~ "," ~ C(x.e2))
      case x:BuiltInObjects.Subtract    => "SUBTRACT " ~ paren(C(x.e1) ~ "," ~ C(x.e2))
      case x:BuiltInObjects.Tokenize    => "TOKENIZE " ~ paren(C(x.expr) ~ opt(x.delimiter.map(y => "," ~ C(y))))
      // Built In Math Objects
      case BuiltInObjects.Random        => "RANDOM()"
      case x:BuiltInObjects.RoundTo     => "RoundTo" ~ paren(C(x.e1) ~ "," ~ C(x.e2) ~ opt(x.e3.map(y => "," ~ C(y))))

      case x:EO.Flatten                 => " FLATTEN " ~ paren(C(x.expr))
      case x:PigAlias.Dollar            => Template.StringToTemplate("$" + s"${x.value}")

      case x:PigAlias.Assign      => C(x.lhs) ~ " = " ~ C(x.rhs)
      case x:PigAlias.PigSymbol   => x.symbol.toString().substring(1)
      case x:PigAlias.SymbolBy    => C(x.lhs) ~ " by " ~ C(x.rhs)
      case x:PigAlias.SymbolByDirection => C(x.symbol) ~ Template.StringToTemplate(x.text)






      case   StateObjects.Collected   => Template.StringToTemplate("'collected'")
      case   StateObjects.Merge       => Template.StringToTemplate("'merge'")
      case x:StateObjects.Import      => "IMPORT" ~ surround(x.input,"\"")








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
      case   NewSchema.Map     => Template.StringToTemplate("map[]")
      case x:NewSchema.TupleTrait   => createSchema(x)
      case _                   => Template.StringToTemplate("error")
    }
  }


  def orderBy(orderBy:StateObjects.OrderBy) = {
    def by(input:PigExpression) = new Template.StringValue(input.create.name)
    "order " ~ C(orderBy.input) ~ " by " ~ sep(orderBy.inputs.map(by(_)),",") ~ par(orderBy.par)
  }



  def schema(schema:NewSchema) = {
    paren(commaSep(schema.newItems.map(createSchema(_)).toList))
  }



  def loader(model:StateObjects.Loader) =
    "LOAD " ~ surround(model.ident,"'","' ") ~ use(model.usingModel) ~ as(model.schema)


  def store(model:StateObjects.Store) = {
    "STORE " ~ C(model.input) ~ "INTO " ~ surround(model.intoModel.get,"'","' ") ~ use(model.usingModel)
  }

  def cross(model:StateObjects.Cross) = {
    "CROSS " ~ sep(model.inputs.map(C(_)),",") ~ partitionBy(model.partitionBy) ~ par(model.par)
  }




  def cube(model:StateObjects.CubeBy) = {
    "CUBE " ~ C(model.input) ~ " by " ~ sep(model.inputs.map(C(_)),",") ~ par(model.par)
  }

  def cubeI(name:String,model:StateObjects.CubeInput) = {
    val temp = Template.L(model.input.map(C(_)))
    name ~ " " ~ surround(sep(temp,","),"(",")")
  }

  def distinct(model:StateObjects.Distinct) = {
    "DISTINCT " ~ C(model.input) ~ partitionBy(model.partitionBy) ~ par(model.par)
  }

  def filter(model:StateObjects.FilterBy) = {
    "FILTER " ~ C(model.input) ~ " by " ~ C(model.byTerm)
  }

  def group(model:StateObjects.GroupBy) = {

    "GROUP " ~ sep(model.inputs.map(C(_)),",") ~ pre("using",model.using) ~ partitionBy(model.partitionBy) ~ par(model.par)
  }

  def join(model:StateObjects.Join)     = "JOIN " ~ sep(model.expressions.map(C(_)),",")
  def joinBy(model:StateObjects.JoinBy) = {
    join(model.join) ~ pre("using",model.using) ~ partitionBy(model.partitionBy) ~ par(model.par)
  }


}
