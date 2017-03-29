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

case class Bundle private[impl] 
(
 override val extent: scala.Option[java.util.UUID] /* reference to a resolver.api.Extent */,
 override val kind: gov.nasa.jpl.imce.oml.tables.TerminologyKind,
 override val iri: gov.nasa.jpl.imce.oml.tables.IRI,
 override val annotations: scala.collection.immutable.SortedSet[resolver.api.Annotation],
 override val boxStatements: scala.collection.immutable.SortedSet[resolver.api.TerminologyBoxStatement],
 override val boxAxioms: scala.collection.immutable.SortedSet[resolver.api.TerminologyBoxAxiom],
 override val bundleStatements: scala.collection.immutable.SortedSet[resolver.api.TerminologyBundleStatement],
 override val bundleAxioms: scala.collection.immutable.SortedSet[resolver.api.TerminologyBundleAxiom]
)
extends resolver.api.Bundle
  with TerminologyBox
{
  override def withAnnotations
  (a: scala.collection.immutable.SortedSet[resolver.api.AnnotationPropertyTable])
  : resolver.api.Bundle
  = {
    copy(annotations = this.annotations ++ resolver.convertToAnnotations(a))
  }
  
  override def annotationsByProperty
  ()
  : scala.collection.immutable.SortedSet[resolver.api.AnnotationPropertyTable]
  = {
    resolver.groupAnnotationsByProperty(annotations)
  }
  
  def withBundleStatements
  (s: scala.collection.immutable.SortedSet[resolver.api.TerminologyBundleStatement])
  : resolver.api.Bundle
  = {
    copy(bundleStatements = this.bundleStatements ++ s)
  }
  
  override def withBoxAxioms
  (s: scala.collection.immutable.SortedSet[resolver.api.TerminologyBoxAxiom])
  : resolver.api.Bundle
  = {
    copy(boxAxioms = this.boxAxioms ++ s)
  }
  
  override def withBoxStatements
  (s: scala.collection.immutable.SortedSet[resolver.api.TerminologyBoxStatement])
  : resolver.api.Bundle
  = {
    copy(boxStatements = this.boxStatements ++ s)
  }
  
  override def everything
  ()
  : scala.collection.immutable.SortedSet[resolver.api.Element]
  = {
    super.everything() ++ bundleStatements + this
  }
  



  override def canEqual(that: scala.Any): scala.Boolean = that match {
  	case _: Bundle => true
  	case _ => false
  }

  override val hashCode
  : scala.Int
  = (extent, kind, iri, annotations, boxStatements, boxAxioms, bundleStatements, bundleAxioms).##

  override def equals(other: scala.Any): scala.Boolean = other match {
	  case that: Bundle =>
	    (that canEqual this) &&
	    (this.extent == that.extent) &&
	    (this.kind == that.kind) &&
	    (this.iri == that.iri) &&
	    (this.annotations == that.annotations) &&
	    (this.boxStatements == that.boxStatements) &&
	    (this.boxAxioms == that.boxAxioms) &&
	    (this.bundleStatements == that.bundleStatements) &&
	    (this.bundleAxioms == that.bundleAxioms)

	  case _ =>
	    false
  }
}
