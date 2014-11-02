package org.simplifide.pig.core

import java.util.Properties

import org.apache.pig.ExecType
import org.apache.pig.impl.PigContext
import org.apache.pig.impl.util.PropertiesUtil

/**
 * Created by andy on 11/1/14.
 */
class PigLocalContext(execType: ExecType,properties: Properties) extends PigContext(execType,properties) {
  //this.getProperties.setProperty()
  //addExecTypeProperty(PropertiesUtil.loadDefaultProperties, execTypeString)

}
