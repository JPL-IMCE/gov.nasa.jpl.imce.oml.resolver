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

case class TerminologyGraph private[impl] 
(
 override val uuid: java.util.UUID,
 override val kind: gov.nasa.jpl.imce.omf.schema.tables.TerminologyGraphKind,
 override val name: gov.nasa.jpl.imce.omf.schema.tables.LocalName,
 override val iri: gov.nasa.jpl.imce.omf.schema.tables.IRI,
 override val statements: scala.collection.immutable.Set[_ <: resolver.api.TerminologyStatement]
)
extends resolver.api.TerminologyGraph
  with TerminologyBox
{

  /*
   * c, if there is a unique axiom
   * TerminologyNestingAxiom(nestedTerminology=this, nestingContext=c, nestingTerminology=_)
   */
  override val nestingConcept
  : scala.Option[resolver.api.Concept]
  = {
  			  terminologyAxioms
  			      .selectByKindOf { case tnx:resolver.api.TerminologyNestingAxiom => tnx }
  			      .headOption
  			      .map(_.nestingContext)
  			}
  
  
/*
   * p, if there is a unique axiom
   * TerminologyNestingAxiom(nestedTerminology=this, nestingContext=_, nestingTerminology=p)
   */
  override val nestingParent
  : scala.Option[resolver.api.TerminologyBox]
  = {
  			  terminologyAxioms
  			      .selectByKindOf { case tnx:resolver.api.TerminologyNestingAxiom => tnx }
  			      .headOption
  			      .map(_.nestingTerminology)
  			}
  
  
/*
   * the set e from all axioms
   * TerminologyExtensionAxiom(extendedTerminology=e, extendingTerminology=this)
   */
  override val extendedGraphs
  : scala.collection.immutable.Set[_ <: resolver.api.TerminologyBox]
  = {
  			  terminologyAxioms
  			      .selectByKindOf { case tex:resolver.api.TerminologyExtensionAxiom => tex }
  			      .map(_.extendedTerminology)
  			}
  

}
