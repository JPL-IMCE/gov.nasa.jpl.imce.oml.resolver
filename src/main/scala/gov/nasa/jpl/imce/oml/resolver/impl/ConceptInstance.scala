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

case class ConceptInstance private[impl] 
(
 override val descriptionBox: resolver.api.DescriptionBox,
 override val singletonConceptClassifier: resolver.api.Concept,
 override val name: gov.nasa.jpl.imce.oml.tables.LocalName,
 override val scalarDataPropertyValues: scala.collection.immutable.SortedSet[resolver.api.ScalarDataPropertyValue],
 override val structuredDataPropertyValues: scala.collection.immutable.SortedSet[resolver.api.StructuredDataPropertyValue]
)
extends resolver.api.ConceptInstance
  with ConceptualEntitySingletonInstance
{
  override def calculateUUID
  ()
  : java.util.UUID
  = {
    
    	val namespace = "ConceptInstance(descriptionBox=" + descriptionBox.uuid + ",singletonConceptClassifier="+singletonConceptClassifier.uuid+")"
    	com.fasterxml.uuid.Generators.nameBasedGenerator(com.fasterxml.uuid.impl.NameBasedGenerator.NAMESPACE_URL).generate(namespace)
  }
  
  override def conceptualEntitySingletonClassifier
  ()
  : resolver.api.ConceptualEntity
  = {
    singletonConceptClassifier
  }
  

  override val uuid
  : java.util.UUID
  = {
    calculateUUID()
  }
  


  override def canEqual(that: scala.Any): scala.Boolean = that match {
  	case _: ConceptInstance => true
  	case _ => false
  }

  override val hashCode
  : scala.Int
  = (uuid, descriptionBox, singletonConceptClassifier, name, scalarDataPropertyValues, structuredDataPropertyValues).##

  override def equals(other: scala.Any): scala.Boolean = other match {
	  case that: ConceptInstance =>
	    (that canEqual this) &&
	    (this.uuid == that.uuid) &&
	    (this.descriptionBox == that.descriptionBox) &&
	    (this.singletonConceptClassifier == that.singletonConceptClassifier) &&
	    (this.name == that.name) &&
	    (this.scalarDataPropertyValues == that.scalarDataPropertyValues) &&
	    (this.structuredDataPropertyValues == that.structuredDataPropertyValues)

	  case _ =>
	    false
  }
}
