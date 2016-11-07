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

package gov.nasa.jpl.imce.omf.schema.resolver.impl

import gov.nasa.jpl.imce.omf.schema._

case class ReifiedRelationshipSpecializationAxiom private[impl] 
(
 override val graph: resolver.api.TerminologyBox,
 override val uuid: java.util.UUID,
 override val subRelationship: resolver.api.ReifiedRelationship,
 override val superRelationship: resolver.api.ReifiedRelationship
)
extends resolver.api.ReifiedRelationshipSpecializationAxiom
  with SpecializationAxiom
{

  /*
   * Get the sub (child) entity
   */
  override val child
  : resolver.api.Term
  = {
    subRelationship
  }
  
  
/*
   * Get the super (parent) entity
   */
  override val parent
  : resolver.api.Term
  = {
    superRelationship
  }
  

}
