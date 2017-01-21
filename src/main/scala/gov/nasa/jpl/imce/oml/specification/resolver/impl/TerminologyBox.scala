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

package gov.nasa.jpl.imce.oml.specification.resolver.impl

import gov.nasa.jpl.imce.oml.specification._

trait TerminologyBox
extends resolver.api.TerminologyBox
  with TerminologyThing
  with Resource
{
  /*
   * The subset of statements that are entities.
   */
  def entities
  ()
  : scala.collection.immutable.SortedSet[resolver.api.Entity]
  = {
    boxStatements.selectByKindOf { case e: Entity => e }
  }
  
  /*
   * The subset of statements that are aspects.
   */
  def aspects
  ()
  : scala.collection.immutable.SortedSet[resolver.api.Aspect]
  = {
    boxStatements.selectByKindOf { case a: Aspect => a }
  }
  
  /*
   * The subset of statements that are concepts.
   */
  def concepts
  ()
  : scala.collection.immutable.SortedSet[resolver.api.Concept]
  = {
    boxStatements.selectByKindOf { case c: Concept => c }
  }
  
  /*
   * The subset of statements that are reified relationships.
   */
  def reifiedRelationships
  ()
  : scala.collection.immutable.SortedSet[resolver.api.ReifiedRelationship]
  = {
    boxStatements.selectByKindOf { case rr: ReifiedRelationship => rr }
  }
  
  /*
   * The subset of statements that are unreified relationships.
   */
  def unreifiedRelationships
  ()
  : scala.collection.immutable.SortedSet[resolver.api.UnreifiedRelationship]
  = {
    boxStatements.selectByKindOf { case ur: UnreifiedRelationship => ur }
  }
  
  /*
   * The subset of statements that are data relationships.
   */
  def dataRelationships
  ()
  : scala.collection.immutable.SortedSet[resolver.api.DataRelationship]
  = {
    boxStatements.selectByKindOf { case dr: DataRelationship => dr }
  }
  
  /*
   * A map for the subset of statements that are
   * entity scalar data property terms indexed by their uuid.
   */
  def entityScalarDataProperties
  ()
  : scala.collection.immutable.SortedSet[resolver.api.EntityScalarDataProperty]
  = {
    boxStatements.selectByKindOf { case dp: EntityScalarDataProperty => dp }
  }
  
  /*
   * A map for the subset of statements that are
   * data range terms indexed by their uuid.
   */
  def dataranges
  ()
  : scala.collection.immutable.SortedSet[resolver.api.DataRange]
  = {
    boxStatements.selectByKindOf { case dr: DataRange => dr }
  }
  
  /*
   * A map for the subset of statements that are
   * scalar datatype terms indexed by their uuid.
   */
  def scalars
  ()
  : scala.collection.immutable.SortedSet[resolver.api.Scalar]
  = {
    boxStatements.selectByKindOf { case s: Scalar => s }
  }
  
  /*
   * A map for the subset of statements that are
   * structured datatype terms indexed by their uuid.
   */
  def structures
  ()
  : scala.collection.immutable.SortedSet[resolver.api.Structure]
  = {
    boxStatements.selectByKindOf { case s: Structure => s }
  }
  
  /*
   * The subset of axioms about terms.
   */
  def termAxioms
  ()
  : scala.collection.immutable.SortedSet[resolver.api.TermAxiom]
  = {
    boxStatements.selectByKindOf { case tx: TermAxiom => tx }
  }
  
  def everything
  ()
  : scala.collection.immutable.SortedSet[resolver.api.TerminologyThing]
  = {
    scala.collection.immutable.SortedSet.empty[resolver.api.TerminologyThing] ++ boxStatements + this
  }
  

  override def canEqual(that: scala.Any): scala.Boolean = that match {
  	case _: TerminologyBox => true
  	case _ => false
  }
}
