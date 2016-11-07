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

import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph
import scala.collection.immutable.Set

case class TerminologyContext private[impl]
(g: Graph[TerminologyBox, TerminologyEdge])
extends resolver.api.TerminologyContext
{

  override val nodes
  : Set[_ <: resolver.api.TerminologyBox]
  = g.nodes.toOuter

  override val bottomNodes
  : Set[_ <: resolver.api.TerminologyBox]
  = g.nodes.filter(0 == _.inDegree).toOuterNodes.to[Set]

  override val rootNodes
  : Set[_ <: resolver.api.TerminologyBox]
  = g.nodes.filter(0 == _.outDegree).toOuterNodes.to[Set]
}
