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
   * entities terms indexed by their uuid.
   */
  val entities
  : scala.collection.immutable.Map[java.util.UUID, resolver.api.Entity]
  = {
  			  import scala.Predef.ArrowAssoc
  			  boxStatements
  			  .selectByKindOf { case e: Entity => e }
  			  .map(e => e.uuid -> e)
  			  .toMap
  			}
  
  
/*
   * A map for the subset of statements that are
   * aspects terms indexed by their uuid.
   */
  val aspects
  : scala.collection.immutable.Map[java.util.UUID, resolver.api.Aspect]
  = {
  			  import scala.Predef.ArrowAssoc
  			  boxStatements
  			  .selectByKindOf { case a: Aspect => a }
  			  .map(a => a.uuid -> a)
  			  .toMap
  			}
  
  
/*
   * A map for the subset of statements that are
   * concepts terms indexed by their uuid.
   */
  val concepts
  : scala.collection.immutable.Map[java.util.UUID, resolver.api.Concept]
  = {
  			  import scala.Predef.ArrowAssoc
  			  boxStatements
  			  .selectByKindOf { case c: Concept => c }
  			  .map(c => c.uuid -> c)
  			  .toMap
  			}
  
  
/*
   * A map for the subset of statements that are
   * reified relationship terms indexed by their uuid.
   */
  val reifiedRelationships
  : scala.collection.immutable.Map[java.util.UUID, resolver.api.ReifiedRelationship]
  = {
  			  import scala.Predef.ArrowAssoc
  			  boxStatements
  			  .selectByKindOf { case r: ReifiedRelationship => r }
  			  .map(r => r.uuid -> r)
  			  .toMap
  			}
  
  
/*
   * A map for the subset of statements that are
   * unreified relationship terms indexed by their uuid.
   */
  val unreifiedRelationships
  : scala.collection.immutable.Map[java.util.UUID, resolver.api.UnreifiedRelationship]
  = {
  			  import scala.Predef.ArrowAssoc
  			  boxStatements
  			  .selectByKindOf { case r: UnreifiedRelationship => r }
  			  .map(r => r.uuid -> r)
  			  .toMap
  			}
  
  
/*
   * A map for the subset of statements that are
   * datatype terms indexed by their uuid.
   */
  val dataRelationships
  : scala.collection.immutable.Map[java.util.UUID, resolver.api.DataRelationship]
  = {
  			  import scala.Predef.ArrowAssoc
  			  boxStatements
  			  .selectByKindOf { case dr: DataRelationship => dr }
  			  .map(dr => dr.uuid -> dr)
  			  .toMap
  			}
  
  
/*
   * A map for the subset of statements that are
   * entity scalar data property terms indexed by their uuid.
   */
  val entityScalarDataProperties
  : scala.collection.immutable.Map[java.util.UUID, resolver.api.EntityScalarDataProperty]
  = {
  			  import scala.Predef.ArrowAssoc
  			  boxStatements
  			  .selectByKindOf { case dp: EntityScalarDataProperty => dp }
  			  .map(dp => dp.uuid -> dp)
  			  .toMap
  			}
  
  
/*
   * A map for the subset of statements that are
   * data range terms indexed by their uuid.
   */
  val dataranges
  : scala.collection.immutable.Map[java.util.UUID, resolver.api.DataRange]
  = {
  			  import scala.Predef.ArrowAssoc
  			  boxStatements
  			  .selectByKindOf { case dr: DataRange => dr }
  			  .map(dr => dr.uuid -> dr)
  			  .toMap
  			}
  
  
/*
   * A map for the subset of statements that are
   * scalar datatype terms indexed by their uuid.
   */
  val scalars
  : scala.collection.immutable.Map[java.util.UUID, resolver.api.Scalar]
  = {
  			  import scala.Predef.ArrowAssoc
  			  boxStatements
  			  .selectByKindOf { case s: Scalar => s }
  			  .map(s => s.uuid -> s)
  			  .toMap
  			}
  
  
/*
   * A map for the subset of statements that are
   * structured datatype terms indexed by their uuid.
   */
  val structures
  : scala.collection.immutable.Map[java.util.UUID, resolver.api.Structure]
  = {
  			  import scala.Predef.ArrowAssoc
  			  boxStatements
  			  .selectByKindOf { case s: Structure => s }
  			  .map(s => s.uuid -> s)
  			  .toMap
  			}
  
  
/*
   * The subset of axioms about terms.
   */
  val termAxioms
  : scala.collection.immutable.Set[_ <: resolver.api.TermAxiom]
  = {
  			  boxStatements.selectByKindOf { case tx: TermAxiom => tx }
  			}
  

}
