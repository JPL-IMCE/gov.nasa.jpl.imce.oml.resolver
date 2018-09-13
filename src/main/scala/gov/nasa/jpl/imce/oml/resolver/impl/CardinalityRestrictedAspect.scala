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

case class CardinalityRestrictedAspect private[impl] 
	(
	 override val uuid: resolver.api.taggedTypes.CardinalityRestrictedAspectUUID,
	 override val restrictedRange: scala.Option[resolver.api.Entity],
	 override val name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName,
	 override val restrictedCardinality: gov.nasa.jpl.imce.oml.tables.taggedTypes.PositiveIntegerLiteral,
	 override val restrictedRelationship: resolver.api.RestrictableRelationship,
	 override val restrictionKind: gov.nasa.jpl.imce.oml.tables.CardinalityRestrictionKind
)
extends resolver.api.CardinalityRestrictedAspect
  with AspectKind
{

  override def canEqual(that: scala.Any): scala.Boolean = that match {
	  case _: CardinalityRestrictedAspect => true
 	  case _ => false
  }

  override val hashCode
  : scala.Int
  = (uuid, restrictedRange, name, restrictedCardinality, restrictedRelationship, restrictionKind).##

  override def equals(other: scala.Any): scala.Boolean = other match {
    case that: CardinalityRestrictedAspect =>
      (that canEqual this) &&
      (this.uuid == that.uuid) &&
      (this.restrictedRange == that.restrictedRange) &&
      (this.name == that.name) &&
      (this.restrictedCardinality == that.restrictedCardinality) &&
      (this.restrictedRelationship == that.restrictedRelationship) &&
      (this.restrictionKind == that.restrictionKind)

    case _ =>
      false
  }
}
