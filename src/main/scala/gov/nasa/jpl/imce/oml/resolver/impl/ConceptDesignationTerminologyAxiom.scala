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
 override val tbox: resolver.api.TerminologyBox,
 override val designatedConcept: resolver.api.Concept,
 override val designatedTerminology: resolver.api.TerminologyBox
)
extends resolver.api.ConceptDesignationTerminologyAxiom
  with TerminologyBoxAxiom
{
  override def calculateUUID
  ()
  : java.util.UUID
  = {
    
    	val namespace = "ConceptDesignationTerminologyAxiom(designationTerminologyGraph=" + tbox.uuid + ",designatedConcept="+designatedConcept.uuid+")"
    	com.fasterxml.uuid.Generators.nameBasedGenerator(com.fasterxml.uuid.impl.NameBasedGenerator.NAMESPACE_URL).generate(namespace)
  }
  
  def designationTerminologyGraph
  ()
  : resolver.api.TerminologyGraph
  = {
    tbox match { case g: TerminologyGraph => g }
  }
  
  /*
   * The designationTerminologyGraph is the source
   */
  override def source
  ()
  : resolver.api.TerminologyBox
  = {
    tbox
  }
  
  /*
   * The TerminologyBox that asserts the designatedConcept is the target
   */
  override def target
  ()
  : resolver.api.TerminologyBox
  = {
    designatedTerminology
  }
  

  override val uuid
  : java.util.UUID
  = {
    calculateUUID()
  }
  


  override def canEqual(that: scala.Any): scala.Boolean = that match {
  	case _: ConceptDesignationTerminologyAxiom => true
  	case _ => false
  }

  override val hashCode
  : scala.Int
  = (uuid, tbox, designatedConcept, designatedTerminology).##

  override def equals(other: scala.Any): scala.Boolean = other match {
	  case that: ConceptDesignationTerminologyAxiom =>
	    (that canEqual this) &&
	    (this.uuid == that.uuid) &&
	    (this.tbox == that.tbox) &&
	    (this.designatedConcept == that.designatedConcept) &&
	    (this.designatedTerminology == that.designatedTerminology)

	  case _ =>
	    false
  }
}
