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

case class StructuredDataPropertyTuple private[impl] 
	(
	 override val uuid: resolver.api.taggedTypes.StructuredDataPropertyTupleUUID,
	 override val structuredDataProperty: resolver.api.DataRelationshipToStructure
)
extends resolver.api.StructuredDataPropertyTuple
  with SingletonInstanceStructuredDataPropertyContext
{

  override def descriptionBox
  ()(implicit extent: resolver.api.Extent)
	  : scala.Option[resolver.api.DescriptionBox]
	  = {
	    extent.singletonInstanceStructuredDataPropertyContextOfStructuredDataPropertyTuple.get(this).flatMap(_.descriptionBox())
	  }

  override def canEqual(that: scala.Any): scala.Boolean = that match {
	  case _: StructuredDataPropertyTuple => true
 	  case _ => false
  }

  override val hashCode
  : scala.Int
  = (uuid, structuredDataProperty).##

  override def equals(other: scala.Any): scala.Boolean = other match {
    case that: StructuredDataPropertyTuple =>
      (that canEqual this) &&
      (this.uuid == that.uuid) &&
      (this.structuredDataProperty == that.structuredDataProperty)

    case _ =>
      false
  }
}
