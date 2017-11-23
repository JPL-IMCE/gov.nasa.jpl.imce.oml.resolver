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

case class IRIScalarRestriction private[impl] 
	(
	 override val uuid: resolver.api.taggedTypes.IRIScalarRestrictionUUID,
	 override val restrictedRange: resolver.api.DataRange,
	 override val length: scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.PositiveIntegerLiteral],
	 override val minLength: scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.PositiveIntegerLiteral],
	 override val maxLength: scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.PositiveIntegerLiteral],
	 override val name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName,
	 override val pattern: scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.LiteralPattern]
)
extends resolver.api.IRIScalarRestriction
  with RestrictedDataRange
{

  override def canEqual(that: scala.Any): scala.Boolean = that match {
	  case _: IRIScalarRestriction => true
 	  case _ => false
  }

  override val hashCode
  : scala.Int
  = (uuid, restrictedRange, length, minLength, maxLength, name, pattern).##

  override def equals(other: scala.Any): scala.Boolean = other match {
    case that: IRIScalarRestriction =>
      (that canEqual this) &&
      (this.uuid == that.uuid) &&
      (this.restrictedRange == that.restrictedRange) &&
      (this.length == that.length) &&
      (this.minLength == that.minLength) &&
      (this.maxLength == that.maxLength) &&
      (this.name == that.name) &&
      (this.pattern == that.pattern)

    case _ =>
      false
  }
}
