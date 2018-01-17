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

case class SegmentPredicate private[impl] 
	(
	 override val uuid: resolver.api.taggedTypes.SegmentPredicateUUID,
	 override val bodySegment: resolver.api.RuleBodySegment,
	 override val predicate: scala.Option[resolver.api.Predicate],
	 override val reifiedRelationshipSource: scala.Option[resolver.api.ReifiedRelationship],
	 override val reifiedRelationshipInverseSource: scala.Option[resolver.api.ReifiedRelationship],
	 override val reifiedRelationshipTarget: scala.Option[resolver.api.ReifiedRelationship],
	 override val reifiedRelationshipInverseTarget: scala.Option[resolver.api.ReifiedRelationship],
	 override val unreifiedRelationshipInverse: scala.Option[resolver.api.UnreifiedRelationship]
)
extends resolver.api.SegmentPredicate
  with ElementCrossReferenceTuple
{

  def termPredicate
  ()
	  : scala.Option[resolver.api.Term]
	  = {
	    predicate.map(_.term()) orElse 
	    	reifiedRelationshipSource orElse 
	    	reifiedRelationshipInverseSource orElse
	    	reifiedRelationshipTarget orElse
	    	reifiedRelationshipInverseTarget orElse
	    	unreifiedRelationshipInverse
	  }

  def moduleContext
  ()(implicit extent: resolver.api.Extent)
	  : scala.Option[resolver.api.Module]
	  = {
	    bodySegment.moduleContext()
	  }

  override def canEqual(that: scala.Any): scala.Boolean = that match {
	  case _: SegmentPredicate => true
 	  case _ => false
  }

  override val hashCode
  : scala.Int
  = (uuid, bodySegment, predicate, reifiedRelationshipSource, reifiedRelationshipInverseSource, reifiedRelationshipTarget, reifiedRelationshipInverseTarget, unreifiedRelationshipInverse).##

  override def equals(other: scala.Any): scala.Boolean = other match {
    case that: SegmentPredicate =>
      (that canEqual this) &&
      (this.uuid == that.uuid) &&
      (this.bodySegment == that.bodySegment) &&
      (this.predicate == that.predicate) &&
      (this.reifiedRelationshipSource == that.reifiedRelationshipSource) &&
      (this.reifiedRelationshipInverseSource == that.reifiedRelationshipInverseSource) &&
      (this.reifiedRelationshipTarget == that.reifiedRelationshipTarget) &&
      (this.reifiedRelationshipInverseTarget == that.reifiedRelationshipInverseTarget) &&
      (this.unreifiedRelationshipInverse == that.unreifiedRelationshipInverse)

    case _ =>
      false
  }
}
