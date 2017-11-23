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

case class UnreifiedRelationshipPropertyPredicate private[impl] 
	(
	 override val uuid: resolver.api.taggedTypes.UnreifiedRelationshipPropertyPredicateUUID,
	 override val unreifiedRelationship: resolver.api.UnreifiedRelationship,
	 override val bodySegment: resolver.api.RuleBodySegment
)
extends resolver.api.UnreifiedRelationshipPropertyPredicate
  with BinarySegmentForwardPropertyPredicate
{

  override def termPredicate
  ()
	  : resolver.api.Term
	  = {
	    unreifiedRelationship
	  }

  override def canEqual(that: scala.Any): scala.Boolean = that match {
	  case _: UnreifiedRelationshipPropertyPredicate => true
 	  case _ => false
  }

  override val hashCode
  : scala.Int
  = (uuid, unreifiedRelationship, bodySegment).##

  override def equals(other: scala.Any): scala.Boolean = other match {
    case that: UnreifiedRelationshipPropertyPredicate =>
      (that canEqual this) &&
      (this.uuid == that.uuid) &&
      (this.unreifiedRelationship == that.unreifiedRelationship) &&
      (this.bodySegment == that.bodySegment)

    case _ =>
      false
  }
}
