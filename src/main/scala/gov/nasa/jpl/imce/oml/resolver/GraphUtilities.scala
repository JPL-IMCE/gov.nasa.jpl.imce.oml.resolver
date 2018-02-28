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

package gov.nasa.jpl.imce.oml.resolver

import gov.nasa.jpl.imce.oml.parallelSort

import scala.collection.immutable.{::, List, Nil, Seq, Set, Vector}
import scala.reflect.ClassTag
import scala.{Boolean,Ordering}
import scala.Predef.{identity,require}
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalaz._
import Scalaz._

/**
  * Hierarchical Topological Sort for directed hypergraphs where a node is a graph.
  */
object GraphUtilities {

  protected def subGraphPrecedence
  [N: ClassTag, E[M] <: DiEdge[M]]
  (g: Graph[N, E])
  (lt: Graph[N, E], gt: Graph[N, E])
  (implicit nOrder: Ordering[N])
  : Boolean
  = {
    require(lt.nonEmpty)
    require(gt.nonEmpty)

    val before =
      lt.toOuterNodes.exists { ln =>
        val n1 = g.get(ln)
        gt.toOuterNodes.exists { rn =>
          val n2 = g.get(rn)
          n1.isPredecessorOf(n2)
        }
      }

    if (before)
      true
    else {
      val inverse =
        lt.toOuterNodes.exists { ln =>
          val n1 = g.get(ln)
          gt.toOuterNodes.exists { rn =>
            val n2 = g.get(rn)
            n2.isPredecessorOf(n1)
          }
        }

      if (inverse)
        false
      else {
        val lns = parallelSort.parSortBy(lt.toOuterNodes.to[Vector], identity[N])
        val rns = parallelSort.parSortBy(gt.toOuterNodes.to[Vector], identity[N])
        nOrder.compare(lns.head, rns.head) <= 0
      }
    }
  }

  /**
    * Hierarchical Topological Sort of a Hypergraph.
    *
    * @param queue A collection of graphs, each of type `Graph[N, E]`
    * @param nOrder Ordering among graph nodes of type `N`
    * @tparam N Type of graph nodes
    * @tparam E Type of graph edges
    * @return If successful, the topological sort of all the nodes of the graphs in `queue`.
    */
  def hierarchicalTopologicalSort[N: ClassTag, E[M] <: DiEdge[M]]
  (queue: Seq[Graph[N, E]])
  (implicit nOrder: Ordering[N])
  : Set[java.lang.Throwable] \/ Seq[N]
  = hierarchicalTopologicalSort(queue, Seq.empty)

  @scala.annotation.tailrec
  protected final def hierarchicalTopologicalSort[N: ClassTag, E[M] <: DiEdge[M]]
  (queue: Seq[Graph[N, E]], result: Seq[N])
  (implicit nOrder: Ordering[N])
  : Set[java.lang.Throwable] \/ Seq[N]
  = queue match {
    case Nil =>
      result.right
    case g :: gs =>

      if (g.isAcyclic) {
        val gsorted: Seq[N] = g.topologicalSort().right.get.toOuter.toOuter.to[Seq]
        hierarchicalTopologicalSort(gs, result ++ gsorted)
      } else {
        val sccs1 = g.strongComponentTraverser()
        val sccs2 = sccs1.map(_.toGraph)
        val sccs3 = sccs2.to[List]
        val sccs4 = sccs3.filter(_.nonEmpty)
        val sccs = parallelSort
          .parSortBy(sccs4, identity[Graph[N, E]])(Ordering.fromLessThan(subGraphPrecedence(g)))

        sccs match {
          case Nil =>
            result.right[Set[java.lang.Throwable]]

          case n :: ns =>
            if (n.isAcyclic) {
              val nsorted: Seq[N] = n.topologicalSort().right.get.toOuter.toOuter.to[Seq]
              hierarchicalTopologicalSort(ns ++ gs, result ++ nsorted)
            } else {
              val ncomponent: Seq[N] = n.toOuterNodes.to[Seq]
              val gn = g -- n
              hierarchicalTopologicalSort(gn +: (ns ++ gs), result ++ ncomponent)
            }
        }
      }
  }

}
