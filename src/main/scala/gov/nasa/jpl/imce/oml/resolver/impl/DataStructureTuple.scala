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

case class DataStructureTuple private[impl] 
(
 override val dataStructureType: resolver.api.Structure,
 override val structuredDataPropertyValue: scala.Option[java.util.UUID] /* reference to a resolver.api.StructuredDataPropertyValue */,
 override val name: gov.nasa.jpl.imce.oml.tables.LocalName,
 override val scalarDataPropertyValues: scala.collection.immutable.SortedSet[resolver.api.ScalarDataPropertyValue],
 override val structuredDataPropertyValues: scala.collection.immutable.SortedSet[resolver.api.StructuredDataPropertyValue]
)
extends resolver.api.DataStructureTuple
  with SingletonInstance
{



  override def canEqual(that: scala.Any): scala.Boolean = that match {
  	case _: DataStructureTuple => true
  	case _ => false
  }

  override val hashCode
  : scala.Int
  = (dataStructureType, structuredDataPropertyValue, name, scalarDataPropertyValues, structuredDataPropertyValues).##

  override def equals(other: scala.Any): scala.Boolean = other match {
	  case that: DataStructureTuple =>
	    (that canEqual this) &&
	    (this.dataStructureType == that.dataStructureType) &&
	    (this.structuredDataPropertyValue == that.structuredDataPropertyValue) &&
	    (this.name == that.name) &&
	    (this.scalarDataPropertyValues == that.scalarDataPropertyValues) &&
	    (this.structuredDataPropertyValues == that.structuredDataPropertyValues)

	  case _ =>
	    false
  }
}
