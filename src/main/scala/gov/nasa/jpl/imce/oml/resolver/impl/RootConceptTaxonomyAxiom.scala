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

case class RootConceptTaxonomyAxiom private[impl] 
(
 override val bundle: scala.Option[java.util.UUID] /* reference to a resolver.api.Bundle */,
 override val root: resolver.api.Concept
)
extends resolver.api.RootConceptTaxonomyAxiom
  with TerminologyBundleStatement
  with ConceptTreeDisjunction
{
  override def uuid
  (extent: resolver.api.Extent)
  : scala.Option[java.util.UUID]
  = {
    
    	for {
    	  u1 <- bundle
    	  u2 <- disjointTaxonomyParent.uuid(extent)
    	  u3 <- root.uuid(extent)
    	} yield gov.nasa.jpl.imce.oml.uuid.OMLUUIDGenerator.derivedUUID(
    		"RootConceptTaxonomyAxiom",
    	    "bundle"->u1,
    		"disjointTaxonomyParent"->u2,
    		"root"->u3)
  }
  



  override def canEqual(that: scala.Any): scala.Boolean = that match {
  	case _: RootConceptTaxonomyAxiom => true
  	case _ => false
  }

  override val hashCode
  : scala.Int
  = (bundle, root).##

  override def equals(other: scala.Any): scala.Boolean = other match {
	  case that: RootConceptTaxonomyAxiom =>
	    (that canEqual this) &&
	    (this.bundle == that.bundle) &&
	    (this.root == that.root)

	  case _ =>
	    false
  }
}
