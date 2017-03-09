/*
 * Copyright 2016 California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * License Terms
 */

package gov.nasa.jpl.imce.oml.resolver.impl

import gov.nasa.jpl.imce.oml._

case class TerminologyNestingAxiom private[impl] 
(
 override val tbox: resolver.api.TerminologyBox,
 override val nestingTerminology: resolver.api.TerminologyBox,
 override val nestingContext: resolver.api.Concept
)
extends resolver.api.TerminologyNestingAxiom
  with TerminologyBoxAxiom
{
  override def calculateUUID
  ()
  : java.util.UUID
  = {
    
    	val namespace = "TerminologyNestingAxiom(source=" + source.uuid + ",target="+target.uuid + ",nestingContext="+nestingContext.uuid + ")"
    	com.fasterxml.uuid.Generators.nameBasedGenerator(com.fasterxml.uuid.impl.NameBasedGenerator.NAMESPACE_URL).generate(namespace)
  }
  
  def nestedTerminology
  ()
  : resolver.api.TerminologyGraph
  = {
    tbox match { case g: TerminologyGraph => g }
  }
  
  /*
   * The nestedTerminology is the source
   */
  override def source
  ()
  : resolver.api.TerminologyBox
  = {
    tbox
  }
  
  /*
   * The nestingTerminology is the target
   */
  override def target
  ()
  : resolver.api.TerminologyBox
  = {
    nestingTerminology
  }
  

  override val uuid
  : java.util.UUID
  = {
    calculateUUID()
  }
  


  override def canEqual(that: scala.Any): scala.Boolean = that match {
  	case _: TerminologyNestingAxiom => true
  	case _ => false
  }

  override val hashCode
  : scala.Int
  = (uuid, tbox, nestingTerminology, nestingContext).##

  override def equals(other: scala.Any): scala.Boolean = other match {
	  case that: TerminologyNestingAxiom =>
	    (that canEqual this) &&
	    (this.uuid == that.uuid) &&
	    (this.tbox == that.tbox) &&
	    (this.nestingTerminology == that.nestingTerminology) &&
	    (this.nestingContext == that.nestingContext)

	  case _ =>
	    false
  }
}
