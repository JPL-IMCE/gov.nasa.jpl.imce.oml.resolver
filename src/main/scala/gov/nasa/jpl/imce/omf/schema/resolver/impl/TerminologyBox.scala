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

trait TerminologyBox
extends resolver.api.TerminologyBox
  with TerminologyThing
  with Resource
{

  /*
   * A map for the subset of statements that are
   * entities terms indexed by their iri.
   */
  override val entities
  : scala.collection.immutable.Map[java.net.URI, resolver.api.Entity]
  = {
  			  import scala.Predef.ArrowAssoc
  			  statements
  			  .selectByKindOf { case e: Entity => e }
  			  .map(e => java.net.URI.create(e.iri) -> e)
  			  .toMap
  			}
  
  
/*
   * A map for the subset of statements that are
   * datatype terms indexed by their iri.
   */
  override val dataRelationships
  : scala.collection.immutable.Map[java.net.URI, resolver.api.DataRelationship]
  = {
  			  import scala.Predef.ArrowAssoc
  			  statements
  			  .selectByKindOf { case dr: DataRelationship => dr }
  			  .map(dr => java.net.URI.create(dr.iri) -> dr)
  			  .toMap
  			}
  
  
/*
   * A map for the subset of statements that are
   * datatype terms indexed by their iri.
   */
  override val datatypes
  : scala.collection.immutable.Map[java.net.URI, resolver.api.Datatype]
  = {
  			  import scala.Predef.ArrowAssoc
  			  statements
  			  .selectByKindOf { case dt: Datatype => dt }
  			  .map(dt => java.net.URI.create(dt.iri) -> dt)
  			  .toMap
  			}
  
  
/*
   * The subset of axioms about terminologies.
   */
  override val terminologyAxioms
  : scala.collection.immutable.Set[_ <: resolver.api.TerminologyAxiom]
  = {
  			  statements.selectByKindOf { case tx: TerminologyAxiom => tx }
  			}
  
  
/*
   * The subset of axioms about terms.
   */
  override val termAxioms
  : scala.collection.immutable.Set[_ <: resolver.api.TermAxiom]
  = {
  			  statements.selectByKindOf { case tx: TermAxiom => tx }
  			}
  

}
