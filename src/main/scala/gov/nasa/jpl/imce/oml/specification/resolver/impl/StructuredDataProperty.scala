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

case class StructuredDataProperty private[impl] 
(
 override val graph: resolver.api.TerminologyBox,
 override val uuid: java.util.UUID,
 override val name: gov.nasa.jpl.imce.oml.specification.tables.LocalName,
 override val domain: resolver.api.Structure,
 override val range: resolver.api.Structure
)
extends resolver.api.StructuredDataProperty
  with DataRelationship
  with DataRelationshipFromStructure
  with DataRelationshipToStructure
{
  override def source
  ()
  : resolver.api.Term
  = {
    domain
  }
  
  override def target
  ()
  : resolver.api.Datatype
  = {
    range
  }
  

  override def canEqual(that: scala.Any): scala.Boolean = that match {
  	case _: StructuredDataProperty => true
  	case _ => false
  }

  override val hashCode
  : scala.Int
  = (graph, uuid, name, domain, range).##

  override def equals(other: scala.Any): scala.Boolean = other match {
	  case that: StructuredDataProperty =>
	    (that canEqual this) &&
	    (this.graph == that.graph) &&
	    (this.uuid == that.uuid) &&
	    (this.name == that.name) &&
	    (this.domain == that.domain) &&
	    (this.range == that.range)

	  case _ =>
	    false
  }
}
