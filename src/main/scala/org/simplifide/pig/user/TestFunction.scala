package org.simplifide.pig.user

import org.apache.pig.impl.logicalLayer.schema.Schema
import org.apache.pig.impl.logicalLayer.schema.Schema.FieldSchema
import org.apache.pig.{FuncSpec, EvalFunc}
import org.apache.pig.data.{DataType, Tuple}

/**
 * Created by andy on 10/29/14.
 */
class TestFunction extends EvalFunc[String]{

  def exec(input:Tuple): String = {
    if (input == null || input.size() == 0 ) ""
    else input.get(0).toString.toUpperCase;
  }
  def create = new FuncSpec(this.getClass.getName,new Schema(new Schema.FieldSchema(null,DataType.CHARARRAY)))

}

object TestFunction {

}
