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

case class RuleBodySegment private[impl] 
	(
	 override val uuid: resolver.api.taggedTypes.RuleBodySegmentUUID,
	 override val previousSegment: scala.Option[resolver.api.RuleBodySegment],
	 override val rule: scala.Option[resolver.api.ChainRule]
)
extends resolver.api.RuleBodySegment
  with Element
{

  def position
  ()
	  : scala.Int
	  = {
	    previousSegment match { 
	    					case scala.None => 
	    						1
	    					case scala.Some(prev) => 
	    						1 + prev.position()
	    				}
	  }

  def chainRule
  ()
	  : resolver.api.ChainRule
	  = {
	    rule match {
	    					case scala.Some(r) =>
	    						r
	    					case scala.None =>
	    						previousSegment match {
	    							case scala.Some(prev) =>
	    								prev.chainRule()
	    							case scala.None =>
	    								throw new java.lang.IllegalArgumentException("chainRule() must be well-defined")
	    						}				}
	  }

  def moduleContext
  ()(implicit extent: resolver.api.Extent)
	  : scala.Option[resolver.api.Module]
	  = {
	    chainRule().moduleContext()
	  }

  override def canEqual(that: scala.Any): scala.Boolean = that match {
	  case _: RuleBodySegment => true
 	  case _ => false
  }

  override val hashCode
  : scala.Int
  = (uuid, previousSegment, rule).##

  override def equals(other: scala.Any): scala.Boolean = other match {
    case that: RuleBodySegment =>
      (that canEqual this) &&
      (this.uuid == that.uuid) &&
      (this.previousSegment == that.previousSegment) &&
      (this.rule == that.rule)

    case _ =>
      false
  }
}
