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

case class TerminologyExtensionAxiom private[impl] 
(
 override val extendedTerminology: resolver.api.TerminologyBox
)
extends resolver.api.TerminologyExtensionAxiom
  with TerminologyBoxAxiom
{
  override def uuid
  ()(implicit extent: Extent)
  : scala.Option[java.util.UUID]
  = {
    
    	for {
    	  s <- extent.lookupModule(tbox)
    	  u1 <- s.uuid(extent)
        	  u2 <- extendedTerminology.uuid(extent)
    	} yield gov.nasa.jpl.imce.oml.uuid.OMLUUIDGenerator.derivedUUID("TerminologyExtensionAxiom","source"->u1,"target"->u2)
  }
  
  def extendingTerminology
  ()(implicit extent: Extent)
  : scala.Option[resolver.api.TerminologyBox]
  = {
    resolver.OMLOps.lookupTerminologyBox(extent, tbox)
  }
  
  /*
   * The extendingTerminology is the source
   */
  override def source
  ()(implicit extent: Extent)
  : scala.Option[resolver.api.TerminologyBox]
  = {
    extendingTerminology
  }
  
  /*
   * The extendedTerminology is the target
   */
  override def target
  ()(implicit extent: Extent)
  : resolver.api.TerminologyBox
  = {
    extendedTerminology
  }
  



  override def canEqual(that: scala.Any): scala.Boolean = that match {
  	case _: TerminologyExtensionAxiom => true
  	case _ => false
  }

  override val hashCode
  : scala.Int
  = (extendedTerminology).##

  override def equals(other: scala.Any): scala.Boolean = other match {
	  case that: TerminologyExtensionAxiom =>
	    (that canEqual this) &&
	    (this.extendedTerminology == that.extendedTerminology)

	  case _ =>
	    false
  }
}
