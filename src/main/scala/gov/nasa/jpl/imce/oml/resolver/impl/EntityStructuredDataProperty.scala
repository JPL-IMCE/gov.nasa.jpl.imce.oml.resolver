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

case class EntityStructuredDataProperty private[impl] 
(
 override val tbox: scala.Option[java.util.UUID] /* reference to a resolver.api.TerminologyBox */,
 override val domain: resolver.api.Entity,
 override val range: resolver.api.Structure,
 override val isIdentityCriteria: scala.Boolean,
 override val name: gov.nasa.jpl.imce.oml.tables.LocalName
)
extends resolver.api.EntityStructuredDataProperty
  with DataRelationship
  with DataRelationshipFromEntity
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
  	case _: EntityStructuredDataProperty => true
  	case _ => false
  }

  override val hashCode
  : scala.Int
  = (tbox, domain, range, isIdentityCriteria, name).##

  override def equals(other: scala.Any): scala.Boolean = other match {
	  case that: EntityStructuredDataProperty =>
	    (that canEqual this) &&
	    (this.tbox == that.tbox) &&
	    (this.domain == that.domain) &&
	    (this.range == that.range) &&
	    (this.isIdentityCriteria == that.isIdentityCriteria) &&
	    (this.name == that.name)

	  case _ =>
	    false
  }
}
