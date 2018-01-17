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

trait EntityRestrictionAxiom
extends resolver.api.EntityRestrictionAxiom
  with TermAxiom
  with ElementCrossReferenceTuple
{
  override val uuid: resolver.api.taggedTypes.EntityRestrictionAxiomUUID
  override val restrictedRelationship: resolver.api.RestrictableRelationship
  override val restrictedRange: resolver.api.Entity
  override val restrictedDomain: resolver.api.Entity

  def allNestedElements
  ()(implicit extent: resolver.api.Extent)
	  : scala.collection.immutable.Set[_ <: resolver.api.LogicalElement]
	  = {
	    scala.collection.immutable.Set.empty[resolver.api.LogicalElement]
	  }

  def restrictedRelation
  ()
	  : resolver.api.EntityRelationship
	  = {
	    restrictedRelationship.relation()
	  }

  override def canEqual(that: scala.Any): scala.Boolean = that match {
	  case _: EntityRestrictionAxiom => true
 	  case _ => false
  }

}
