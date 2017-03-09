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

case class ReifiedRelationshipInstance private[impl] 
(
 override val descriptionBox: resolver.api.DescriptionBox,
 override val singletonReifiedRelationshipClassifier: resolver.api.ReifiedRelationship,
 override val name: gov.nasa.jpl.imce.oml.tables.LocalName,
 override val scalarDataPropertyValues: scala.collection.immutable.SortedSet[resolver.api.ScalarDataPropertyValue],
 override val structuredDataPropertyValues: scala.collection.immutable.SortedSet[resolver.api.StructuredDataPropertyValue]
)
extends resolver.api.ReifiedRelationshipInstance
  with ConceptualEntitySingletonInstance
{
  override def calculateUUID
  ()
  : java.util.UUID
  = {
    
    	val namespace = "ReifiedRelationshipInstance(descriptionBox=" + descriptionBox.uuid + ",singletonReifiedRelationshipClassifier="+singletonReifiedRelationshipClassifier.uuid+")"
    	com.fasterxml.uuid.Generators.nameBasedGenerator(com.fasterxml.uuid.impl.NameBasedGenerator.NAMESPACE_URL).generate(namespace)
  }
  
  override def conceptualEntitySingletonClassifier
  ()
  : resolver.api.ConceptualEntity
  = {
    singletonReifiedRelationshipClassifier
  }
  

  override val uuid
  : java.util.UUID
  = {
    calculateUUID()
  }
  


  override def canEqual(that: scala.Any): scala.Boolean = that match {
  	case _: ReifiedRelationshipInstance => true
  	case _ => false
  }

  override val hashCode
  : scala.Int
  = (uuid, descriptionBox, singletonReifiedRelationshipClassifier, name, scalarDataPropertyValues, structuredDataPropertyValues).##

  override def equals(other: scala.Any): scala.Boolean = other match {
	  case that: ReifiedRelationshipInstance =>
	    (that canEqual this) &&
	    (this.uuid == that.uuid) &&
	    (this.descriptionBox == that.descriptionBox) &&
	    (this.singletonReifiedRelationshipClassifier == that.singletonReifiedRelationshipClassifier) &&
	    (this.name == that.name) &&
	    (this.scalarDataPropertyValues == that.scalarDataPropertyValues) &&
	    (this.structuredDataPropertyValues == that.structuredDataPropertyValues)

	  case _ =>
	    false
  }
}
