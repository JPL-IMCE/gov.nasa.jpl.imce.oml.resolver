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

import java.util.UUID

import gov.nasa.jpl.imce.omf.schema._

import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph
import scala.collection.immutable.{Map,Set}

import scala.{None,Option,PartialFunction,Some,StringContext,Tuple2}
import scala.util.{Failure,Success,Try}
import scala.util.control.Exception._
import scala.Predef.{require,ArrowAssoc}

case class TerminologyContext private[resolver]
(g: Graph[resolver.api.TerminologyBox, TerminologyEdge] = Graph[resolver.api.TerminologyBox, TerminologyEdge]())
extends resolver.api.TerminologyContext
{

  def topologicalOrder()
  : Try[g.TopologicalOrder[resolver.api.TerminologyBox]]
  = g
    .topologicalSort()
    .fold[Try[g.TopologicalOrder[resolver.api.TerminologyBox]]](
    (cycleNode: g.NodeT) =>
      Failure(new java.lang.IllegalArgumentException(
        s"TerminologyContext circularity on node: $cycleNode in graph $g")),
    (order: g.TopologicalOrder[g.NodeT]) =>
      Success(order.toOuter))

  def findFirstStartingFrom[T]
  (b: resolver.api.TerminologyBox,
   pf: PartialFunction[resolver.api.TerminologyBox, T])
  : Option[T]
  = g.get(b).outerNodeTraverser.collectFirst(pf)

  override val nodes
  : Map[UUID, resolver.api.TerminologyBox]
  = g.nodes.toOuter.map(t => t.uuid -> t).toMap

  override val graphs
  : Map[UUID, resolver.api.TerminologyGraph]
  = g.nodes.toOuter
    .flatMap {
      case t: resolver.api.TerminologyGraph =>
        Some(t.uuid -> t)
      case _ =>
        None
    }
    .toMap

  override val bundles
  : Map[UUID, resolver.api.Bundle]
  = g.nodes.toOuter
    .flatMap {
      case t: resolver.api.Bundle =>
        Some(t.uuid -> t)
      case _ =>
        None
    }
    .toMap

  override val bottomNodes
  : Set[_ <: resolver.api.TerminologyBox]
  = g.nodes.filter(0 == _.inDegree).toOuterNodes.to[Set]

  override val rootNodes
  : Set[_ <: resolver.api.TerminologyBox]
  = g.nodes.filter(0 == _.outDegree).toOuterNodes.to[Set]

}

object TerminologyContext {

  def replaceNode
  (g: Graph[resolver.api.TerminologyBox, TerminologyEdge],
   prev: resolver.api.TerminologyBox,
   next: resolver.api.TerminologyBox)
  : Try[Graph[resolver.api.TerminologyBox, TerminologyEdge]]
  = g
    .find(outerNode = prev)
    .fold[Try[Graph[resolver.api.TerminologyBox, TerminologyEdge]]](
    Failure(new java.lang.IllegalArgumentException(s"prev node is not in the graph:\nprev:\n$prev\ngraph:\n$g"))
  ) { prevT =>
    nonFatalCatch[Try[Graph[resolver.api.TerminologyBox, TerminologyEdge]]]
      .withApply { (t: java.lang.Throwable) => Failure(t) }
      .apply {
        val in: Set[TerminologyEdge[TerminologyBox]] = prevT.incoming.map { eT =>
          val e = eT.toOuter
          require(e.target == prev)
          e.copy[TerminologyBox](Tuple2(e.source, next))
        }
        val out: Set[TerminologyEdge[TerminologyBox]] = prevT.outgoing.map { eT =>
          val e = eT.toOuter
          require(e.source == prev)
          e.copy[TerminologyBox](Tuple2(next, e.target))
        }

        val g1: Graph[resolver.api.TerminologyBox, TerminologyEdge] = g - prev + next
        val g2: Graph[resolver.api.TerminologyBox, TerminologyEdge] = g1 ++ in ++ out
        Success(g2)
      }
  }

  def initialize()
  : TerminologyContext
  = TerminologyContext()
}
