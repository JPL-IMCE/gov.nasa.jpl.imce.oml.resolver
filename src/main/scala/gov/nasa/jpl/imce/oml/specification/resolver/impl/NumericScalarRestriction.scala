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

case class NumericScalarRestriction private[impl] 
(
 override val graph: resolver.api.TerminologyBox,
 override val uuid: java.util.UUID,
 override val name: gov.nasa.jpl.imce.oml.specification.tables.LocalName,
 override val maxExclusive: scala.Option[gov.nasa.jpl.imce.oml.specification.tables.LexicalNumber],
 override val maxInclusive: scala.Option[gov.nasa.jpl.imce.oml.specification.tables.LexicalNumber],
 override val minExclusive: scala.Option[gov.nasa.jpl.imce.oml.specification.tables.LexicalNumber],
 override val minInclusive: scala.Option[gov.nasa.jpl.imce.oml.specification.tables.LexicalNumber],
 override val restrictedRange: resolver.api.DataRange
)
extends resolver.api.NumericScalarRestriction
  with RestrictedDataRange
{

  override def canEqual(that: scala.Any): scala.Boolean = that match {
  	case _: NumericScalarRestriction => true
  	case _ => false
  }

  override val hashCode
  : scala.Int
  = (graph, uuid, name, maxExclusive, maxInclusive, minExclusive, minInclusive, restrictedRange).##

  override def equals(other: scala.Any): scala.Boolean = other match {
	  case that: NumericScalarRestriction =>
	    (that canEqual this) &&
	    (this.graph == that.graph) &&
	    (this.uuid == that.uuid) &&
	    (this.name == that.name) &&
	    (this.maxExclusive == that.maxExclusive) &&
	    (this.maxInclusive == that.maxInclusive) &&
	    (this.minExclusive == that.minExclusive) &&
	    (this.minInclusive == that.minInclusive) &&
	    (this.restrictedRange == that.restrictedRange)

	  case _ =>
	    false
  }
}
