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

case class ConceptSpecializationAxiom private[impl] 
(
 override val tbox: resolver.api.TerminologyBox,
 override val superConcept: resolver.api.Concept,
 override val subConcept: resolver.api.Concept
)
extends resolver.api.ConceptSpecializationAxiom
  with SpecializationAxiom
{
  override def calculateUUID
  ()
  : java.util.UUID
  = {
    
    	val namespace = "ConceptSpecializationAxiom(subConcept=" + subConcept.uuid + ",superConcept="+superConcept.uuid+")"
    	com.fasterxml.uuid.Generators.nameBasedGenerator(com.fasterxml.uuid.impl.NameBasedGenerator.NAMESPACE_URL).generate(namespace)
  }
  
  /*
   * Get the sub (child) entity
   */
  override def child
  ()
  : resolver.api.Entity
  = {
    subConcept
  }
  
  /*
   * Get the super (parent) entity
   */
  override def parent
  ()
  : resolver.api.Entity
  = {
    superConcept
  }
  

  override val uuid
  : java.util.UUID
  = {
    calculateUUID()
  }
  


  override def canEqual(that: scala.Any): scala.Boolean = that match {
  	case _: ConceptSpecializationAxiom => true
  	case _ => false
  }

  override val hashCode
  : scala.Int
  = (uuid, tbox, superConcept, subConcept).##

  override def equals(other: scala.Any): scala.Boolean = other match {
	  case that: ConceptSpecializationAxiom =>
	    (that canEqual this) &&
	    (this.uuid == that.uuid) &&
	    (this.tbox == that.tbox) &&
	    (this.superConcept == that.superConcept) &&
	    (this.subConcept == that.subConcept)

	  case _ =>
	    false
  }
}
