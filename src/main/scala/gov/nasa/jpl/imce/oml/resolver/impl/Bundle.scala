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
	 override val uuid: resolver.api.taggedTypes.BundleUUID,
	 override val kind: gov.nasa.jpl.imce.oml.tables.TerminologyKind,
	 override val iri: gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI
)
extends resolver.api.Bundle
  with TerminologyBox
{

  override def moduleEdges
  ()(implicit extent: resolver.api.Extent)
	  : scala.collection.immutable.Set[_ <: resolver.api.ModuleEdge]
	  = {
	    extent.lookupBoxAxioms(this) ++ extent.lookupBundleAxioms(this)
	  }

  def moduleElements
  ()(implicit extent: resolver.api.Extent)
	  : scala.collection.immutable.Set[_ <: resolver.api.ModuleElement]
	  = {
	    extent.lookupBundleStatements(this) ++ extent.lookupBoxStatements(this)
	  }

  override def canEqual(that: scala.Any): scala.Boolean = that match {
	  case _: Bundle => true
 	  case _ => false
  }

  override val hashCode
  : scala.Int
  = (uuid, kind, iri).##

  override def equals(other: scala.Any): scala.Boolean = other match {
    case that: Bundle =>
      (that canEqual this) &&
      (this.uuid == that.uuid) &&
      (this.kind == that.kind) &&
      (this.iri == that.iri)

    case _ =>
      false
  }
}
