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

package gov.nasa.jpl.imce.oml.specification.resolver.impl

import gov.nasa.jpl.imce.oml.specification._

case class Bundle private[impl] 
(
 override val uuid: java.util.UUID,
 override val kind: gov.nasa.jpl.imce.oml.specification.tables.TerminologyGraphKind,
 override val name: gov.nasa.jpl.imce.oml.specification.tables.LocalName,
 override val iri: gov.nasa.jpl.imce.oml.specification.tables.IRI,
 override val nsPrefix: gov.nasa.jpl.imce.oml.specification.tables.NamespacePrefix,
 override val annotations: scala.collection.immutable.SortedSet[resolver.api.Annotation],
 override val boxStatements: scala.collection.immutable.SortedSet[resolver.api.TerminologyBoxStatement],
 override val bundleStatements: scala.collection.immutable.SortedSet[resolver.api.TerminologyBundleStatement],
 override val terminologyBoxAxioms: scala.collection.immutable.SortedSet[resolver.api.TerminologyBoxAxiom],
 override val terminologyBundleAxioms: scala.collection.immutable.SortedSet[resolver.api.TerminologyBundleAxiom]
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
  
  override def withBoxStatements
  (s: scala.collection.immutable.SortedSet[resolver.api.TerminologyBoxStatement])
  : resolver.api.Bundle
  = {
    copy(boxStatements = this.boxStatements ++ s)
  }
  
  override def everything
  ()
  : scala.collection.immutable.SortedSet[resolver.api.TerminologyThing]
  = {
    super.everything() ++ bundleStatements + this
  }
  

  override def canEqual(that: scala.Any): scala.Boolean = that match {
  	case _: Bundle => true
  	case _ => false
  }

  override val hashCode
  : scala.Int
  = (uuid, kind, name, iri, nsPrefix, annotations, boxStatements, bundleStatements, terminologyBoxAxioms, terminologyBundleAxioms).##

  override def equals(other: scala.Any): scala.Boolean = other match {
	  case that: Bundle =>
	    (that canEqual this) &&
	    (this.uuid == that.uuid) &&
	    (this.kind == that.kind) &&
	    (this.name == that.name) &&
	    (this.iri == that.iri) &&
	    (this.nsPrefix == that.nsPrefix) &&
	    (this.annotations == that.annotations) &&
	    (this.boxStatements == that.boxStatements) &&
	    (this.bundleStatements == that.bundleStatements) &&
	    (this.terminologyBoxAxioms == that.terminologyBoxAxioms) &&
	    (this.terminologyBundleAxioms == that.terminologyBundleAxioms)

	  case _ =>
	    false
  }
}
