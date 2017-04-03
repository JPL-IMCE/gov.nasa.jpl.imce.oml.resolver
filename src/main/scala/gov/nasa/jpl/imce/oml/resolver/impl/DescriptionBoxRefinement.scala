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

case class DescriptionBoxRefinement private[impl] 
(
 override val refinedDescriptionBox: resolver.api.DescriptionBox
)
extends resolver.api.DescriptionBoxRefinement
  with DescriptionBoxRelationship
{
  override def uuid
  ()(implicit extent: Extent)
  : scala.Option[java.util.UUID]
  = {
    
    	for {
    	  u1 <- refiningDescriptionBox
    	  u2 <- refinedDescriptionBox.uuid(extent)
    	} yield gov.nasa.jpl.imce.oml.uuid.OMLUUIDGenerator.derivedUUID(
    		"DescriptionBoxRefinement",
    	    "refiningDescriptionBox"->u1,
    		"refinedDescriptionBox"->u2)
  }
  
  def descriptionDomain
  ()(implicit extent: Extent)
  : scala.Option[resolver.api.DescriptionBox]
  = {
    resolver.OMLOps.lookupDescriptionBox(extent, refiningDescriptionBox)
  }
  
  def targetModule
  ()
  : resolver.api.Module
  = {
    refinedDescriptionBox
  }
  



  override def canEqual(that: scala.Any): scala.Boolean = that match {
  	case _: DescriptionBoxRefinement => true
  	case _ => false
  }

  override val hashCode
  : scala.Int
  = (refinedDescriptionBox).##

  override def equals(other: scala.Any): scala.Boolean = other match {
	  case that: DescriptionBoxRefinement =>
	    (that canEqual this) &&
	    (this.refinedDescriptionBox == that.refinedDescriptionBox)

	  case _ =>
	    false
  }
}
