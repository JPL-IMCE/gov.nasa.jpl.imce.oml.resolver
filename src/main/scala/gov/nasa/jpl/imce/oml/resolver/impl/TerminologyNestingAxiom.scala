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

import scala.Predef.ArrowAssoc

case class TerminologyNestingAxiom private[impl] 
(
 override val nestingTerminology: resolver.api.TerminologyBox,
 override val nestingContext: resolver.api.Concept
)
extends resolver.api.TerminologyNestingAxiom
  with TerminologyBoxAxiom
{
  override def uuid
  ()(implicit extent: Extent)
  : scala.Option[java.util.UUID]
  = {
    
    	for {
    	  u1 <- tbox
    	  u2 <- nestingTerminology.uuid(extent)
    	  u3 <- nestingContext.uuid(extent)
    	} yield gov.nasa.jpl.imce.oml.uuid.OMLUUIDGenerator.derivedUUID(
    		"TerminologyNestingAxiom",
    	    "tbox"->u1,
    		"nestingTerminology"->u2,
    		"nestingContext"->u3)
  }
  
  def nestedTerminology
  ()(implicit extent: Extent)
  : scala.Option[resolver.api.TerminologyGraph]
  = {
    resolver.OMLOps.lookupTerminologyGraph(extent, tbox)
  }
  
  /*
   * The nestedTerminology is the source
   */
  override def source
  ()(implicit extent: Extent)
  : scala.Option[resolver.api.TerminologyBox]
  = {
    nestedTerminology(extent)
  }
  
  /*
   * The nestingTerminology is the target
   */
  override def target
  ()(implicit extent: Extent)
  : resolver.api.TerminologyBox
  = {
    nestingTerminology
  }
  



  override def canEqual(that: scala.Any): scala.Boolean = that match {
  	case _: TerminologyNestingAxiom => true
  	case _ => false
  }

  override val hashCode
  : scala.Int
  = (nestingTerminology, nestingContext).##

  override def equals(other: scala.Any): scala.Boolean = other match {
	  case that: TerminologyNestingAxiom =>
	    (that canEqual this) &&
	    (this.nestingTerminology == that.nestingTerminology) &&
	    (this.nestingContext == that.nestingContext)

	  case _ =>
	    false
  }
}
