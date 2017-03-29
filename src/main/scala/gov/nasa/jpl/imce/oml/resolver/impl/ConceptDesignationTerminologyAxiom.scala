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

case class ConceptDesignationTerminologyAxiom private[impl] 
(
 override val tbox: scala.Option[java.util.UUID] /* reference to a resolver.api.TerminologyBox */,
 override val designatedConcept: resolver.api.Concept,
 override val designatedTerminology: resolver.api.TerminologyBox
)
extends resolver.api.ConceptDesignationTerminologyAxiom
  with TerminologyBoxAxiom
{
  override def uuid
  (extent: resolver.api.Extent)
  : scala.Option[java.util.UUID]
  = {
    
    	for {
    	  u1 <- tbox
    	  u2 <- designatedTerminology.uuid(extent)
    	  u3 <- designatedConcept.uuid(extent)
    	} yield gov.nasa.jpl.imce.oml.uuid.OMLUUIDGenerator.derivedUUID(
    		"ConceptDesignationTerminologyAxiom",
    	    "tbox"->u1,
    		"designatedTerminology"->u2,
    		"designatedConcept"->u3)
  }
  
  def designationTerminologyGraph
  (extent: resolver.api.Extent)
  : scala.Option[resolver.api.TerminologyGraph]
  = {
    lookupTerminologyGraph(extent, tbox)
  }
  
  /*
   * The designationTerminologyGraph is the source
   */
  override def source
  (extent: resolver.api.Extent)
  : scala.Option[resolver.api.TerminologyBox]
  = {
    designationTerminologyGraph(extent)
  }
  
  /*
   * The TerminologyBox that asserts the designatedConcept is the target
   */
  override def target
  (extent: resolver.api.Extent)
  : resolver.api.TerminologyBox
  = {
    designatedTerminology
  }
  



  override def canEqual(that: scala.Any): scala.Boolean = that match {
  	case _: ConceptDesignationTerminologyAxiom => true
  	case _ => false
  }

  override val hashCode
  : scala.Int
  = (tbox, designatedConcept, designatedTerminology).##

  override def equals(other: scala.Any): scala.Boolean = other match {
	  case that: ConceptDesignationTerminologyAxiom =>
	    (that canEqual this) &&
	    (this.tbox == that.tbox) &&
	    (this.designatedConcept == that.designatedConcept) &&
	    (this.designatedTerminology == that.designatedTerminology)

	  case _ =>
	    false
  }
}
