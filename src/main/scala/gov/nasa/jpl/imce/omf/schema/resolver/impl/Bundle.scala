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

case class Bundle private[impl] 
(
 override val uuid: java.util.UUID,
 override val kind: gov.nasa.jpl.imce.omf.schema.tables.TerminologyGraphKind,
 override val name: gov.nasa.jpl.imce.omf.schema.tables.LocalName,
 override val iri: gov.nasa.jpl.imce.omf.schema.tables.IRI,
 override val annotations: scala.collection.immutable.Set[_ <: resolver.api.Annotation],
 override val boxStatements: scala.collection.immutable.Set[_ <: resolver.api.TerminologyBoxStatement],
 override val bundleStatements: scala.collection.immutable.Set[_ <: resolver.api.TerminologyBundleStatement],
 override val terminologyBundleAxioms: scala.collection.immutable.Set[_ <: resolver.api.TerminologyBundleAxiom]
)
extends resolver.api.Bundle
  with TerminologyBox
{

  def withBundleStatements
  (s: scala.collection.immutable.Set[_ <: resolver.api.TerminologyBundleStatement]
  )
  : resolver.api.Bundle
  = {
  			  copy(bundleStatements = this.bundleStatements ++ s)
  			}
  
  
def withAnnotations
  (a: scala.collection.immutable.Set[_ <: resolver.api.Annotation]
  )
  : resolver.api.Bundle
  = {
  			  copy(annotations = this.annotations ++ a)
  			}
  
  
def withBoxStatements
  (s: scala.collection.immutable.Set[_ <: resolver.api.TerminologyBoxStatement]
  )
  : resolver.api.Bundle
  = {
  			  copy(boxStatements = this.boxStatements ++ s)
  			}
  
  
override def everything
  ()
  : scala.collection.immutable.Set[_ <: resolver.api.TerminologyThing]
  = {
  			  super.everything() ++ bundleStatements + this
  			}
  

}
