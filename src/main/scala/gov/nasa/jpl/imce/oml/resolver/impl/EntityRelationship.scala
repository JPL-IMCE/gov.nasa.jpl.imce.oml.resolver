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

trait EntityRelationship
extends resolver.api.EntityRelationship
  with Term
  with DirectedBinaryRelationshipKind
{
  override val uuid: resolver.api.taggedTypes.EntityRelationshipUUID
  override val source: resolver.api.Entity
  override val target: resolver.api.Entity

  override def relationDomain
  ()
	  : resolver.api.Term
	  = {
	    source
	  }

  override def relationRange
  ()
	  : resolver.api.Term
	  = {
	    target
	  }

  override def canEqual(that: scala.Any): scala.Boolean = that match {
	  case _: EntityRelationship => true
 	  case _ => false
  }

}
