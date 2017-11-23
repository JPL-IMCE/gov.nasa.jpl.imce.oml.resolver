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

case class ConceptPredicate private[impl] 
	(
	 override val uuid: resolver.api.taggedTypes.ConceptPredicateUUID,
	 override val bodySegment: resolver.api.RuleBodySegment,
	 override val concept: resolver.api.Concept
)
extends resolver.api.ConceptPredicate
  with UnarySegmentPredicate
{

  override def termPredicate
  ()
	  : resolver.api.Term
	  = {
	    concept
	  }

  override def canEqual(that: scala.Any): scala.Boolean = that match {
	  case _: ConceptPredicate => true
 	  case _ => false
  }

  override val hashCode
  : scala.Int
  = (uuid, bodySegment, concept).##

  override def equals(other: scala.Any): scala.Boolean = other match {
    case that: ConceptPredicate =>
      (that canEqual this) &&
      (this.uuid == that.uuid) &&
      (this.bodySegment == that.bodySegment) &&
      (this.concept == that.concept)

    case _ =>
      false
  }
}
