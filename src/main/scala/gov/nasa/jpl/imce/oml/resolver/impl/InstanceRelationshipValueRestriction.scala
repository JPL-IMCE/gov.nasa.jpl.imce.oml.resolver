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

case class InstanceRelationshipValueRestriction private[impl] 
	(
	 override val uuid: resolver.api.taggedTypes.InstanceRelationshipValueRestrictionUUID,
	 override val domain: resolver.api.ConceptualEntitySingletonInstance,
	 override val range: resolver.api.ConceptualEntitySingletonInstance,
	 override val restrictedRelationship: resolver.api.RestrictableRelationship
)
extends resolver.api.InstanceRelationshipValueRestriction
  with TerminologyInstanceAssertion
  with ElementCrossReferenceTuple
{

  def descriptionBox
  ()(implicit extent: resolver.api.Extent)
	  : scala.Option[resolver.api.DescriptionBox]
	  = {
	    extent.descriptionBoxOfInstanceRelationshipValueRestriction.get(this)
	  }

  def moduleContext
  ()(implicit extent: resolver.api.Extent)
	  : scala.Option[resolver.api.Module]
	  = {
	    descriptionBox()
	  }

  def allNestedElements
  ()(implicit extent: resolver.api.Extent)
	  : scala.collection.immutable.Set[_ <: resolver.api.LogicalElement]
	  = {
	    scala.collection.immutable.Set.empty[resolver.api.LogicalElement]
	  }

  override def canEqual(that: scala.Any): scala.Boolean = that match {
	  case _: InstanceRelationshipValueRestriction => true
 	  case _ => false
  }

  override val hashCode
  : scala.Int
  = (uuid, domain, range, restrictedRelationship).##

  override def equals(other: scala.Any): scala.Boolean = other match {
    case that: InstanceRelationshipValueRestriction =>
      (that canEqual this) &&
      (this.uuid == that.uuid) &&
      (this.domain == that.domain) &&
      (this.range == that.range) &&
      (this.restrictedRelationship == that.restrictedRelationship)

    case _ =>
      false
  }
}
