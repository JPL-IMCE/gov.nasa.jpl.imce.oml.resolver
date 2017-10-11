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

case class ScalarOneOfLiteralAxiom private[impl] 
	(
	 override val uuid: java.util.UUID,
	 override val axiom: resolver.api.ScalarOneOfRestriction,
	 override val value: gov.nasa.jpl.imce.oml.tables.LiteralValue,
	 override val valueType: scala.Option[resolver.api.DataRange]
)
extends resolver.api.ScalarOneOfLiteralAxiom
  with TermAxiom
{
		
  def allNestedElements
  ()(implicit extent: resolver.api.Extent)
	  : scala.collection.immutable.Set[_ <: resolver.api.Element]
	  = {
	    scala.collection.immutable.Set.empty[resolver.api.Element]
	  }
	  



  override def canEqual(that: scala.Any): scala.Boolean = that match {
  	case _: ScalarOneOfLiteralAxiom => true
  	case _ => false
  }

  override val hashCode
  : scala.Int
  = (uuid, axiom, value, valueType).##

  override def equals(other: scala.Any): scala.Boolean = other match {
   case that: ScalarOneOfLiteralAxiom =>
     (that canEqual this) &&
     (this.uuid == that.uuid) &&
     (this.axiom == that.axiom) &&
     (this.value == that.value) &&
     (this.valueType == that.valueType)

	  case _ =>
	    false
}
}