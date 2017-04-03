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

import scala.Predef.ArrowAssoc

case class BinaryScalarRestriction private[impl] 
(
 override val restrictedRange: resolver.api.DataRange,
 override val length: scala.Option[scala.Int],
 override val minLength: scala.Option[scala.Int],
 override val maxLength: scala.Option[scala.Int],
 override val name: gov.nasa.jpl.imce.oml.tables.LocalName
)
extends resolver.api.BinaryScalarRestriction
  with RestrictedDataRange
{
  override def uuid
  ()(implicit extent: Extent)
  : scala.Option[java.util.UUID]
  = {
    
    	for {
    	  u1 <- tbox
    	  u2 <- restrictedRange.uuid(extent)
    	} yield gov.nasa.jpl.imce.oml.uuid.OMLUUIDGenerator.derivedUUID(
    		"BinaryScalarRestriction",
    	    "tbox"->u1,
    		"restrictedRange"->u2)
  }
  



  override def canEqual(that: scala.Any): scala.Boolean = that match {
  	case _: BinaryScalarRestriction => true
  	case _ => false
  }

  override val hashCode
  : scala.Int
  = (restrictedRange, length, minLength, maxLength, name).##

  override def equals(other: scala.Any): scala.Boolean = other match {
	  case that: BinaryScalarRestriction =>
	    (that canEqual this) &&
	    (this.restrictedRange == that.restrictedRange) &&
	    (this.length == that.length) &&
	    (this.minLength == that.minLength) &&
	    (this.maxLength == that.maxLength) &&
	    (this.name == that.name)

	  case _ =>
	    false
  }
}
