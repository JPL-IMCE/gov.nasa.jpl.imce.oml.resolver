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

import java.util.UUID

import gov.nasa.jpl.imce.oml.covariantTag
import gov.nasa.jpl.imce.oml.covariantTag.@@
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.imce.oml.uuid.JVMUUIDGenerator

import scala.collection.immutable.{Map, Seq, Set}
import scala.{None, Some, StringContext, Unit}
import scala.Predef.{ArrowAssoc, String}
import scalaz._
import Scalaz._
import scalax.collection.GraphEdge.NodeProduct
import scalax.collection.immutable.Graph

/**
  * OML Resolver Support.
  *
  */
object ResolverUtilities {

  type Throwables = Set[java.lang.Throwable]

  /**
    * Convenience conversion of `UUID @@ Tag` to `String @@ Tag` for a given `Tag`.
    *
    * @param uuid Tagged UUID
    * @tparam Tag Tag type
    * @return The `String @@ Tag` representation of `uuid`.
    */
  implicit def toUUIDString[Tag](uuid: UUID @@ Tag)
  : String @@ Tag
  = covariantTag[Tag][String](uuid.toString)

  /**
    * Initialize an OMLTablesResolver for converting OML Tables data to the OML Resolver API.
    *
    * @return An OMLTablesResolver.
    */
  def initializeResolver
  ()
  : Throwables \/ OMLTablesResolver
  = {
    val omlUUIDg = JVMUUIDGenerator()
    val factory = impl.OMLResolvedFactoryImpl(omlUUIDg)
    val init = OMLTablesResolver.initializeTablesResolver(factory)
    init.right
  }

  /**
    * Resolve a sorted collection of pairs of OML Module IRI and corresponding OMLSpecificationTables data.
    *
    * @param r An OMLTablesResolver.
    * @param ts A sequence of pairs of OML Module IRI & corresponding OMLSpecificationTables data.
    * @return A sorted collection of OML Resolver API Extents corresponding to
    *         the OML Modules resolved from their OMLSpecificationTables data.
    */
  def resolveTables
  (r: Throwables \/ OMLTablesResolver, ts: Seq[(tables.taggedTypes.IRI, tables.OMLSpecificationTables)])
  : Throwables \/ Seq[api.Extent]
  = for {
    resolved <- ts.foldLeft[Throwables \/ OMLTablesResolver] {
      initializeResolver()
    } { case (acc, (iri, table)) =>

      for {
        prev <- acc
        current = prev.copy(queue = table)

        res <- OMLTablesResolver.resolve(current)
          .toDisjunction
          .leftMap(Set[java.lang.Throwable](_))

        _ <- if (!res.queue.isEmpty)
          Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
            s"Conversion of $iri incomplete:\n"+res.queue.show
          )).left[Unit]
        else
          ().right[Throwables]

        next = OMLTablesResolver.accumulateResultContext(res)
      } yield next
    }

    extents = resolved.otherContexts // allContexts === Extent.empty ++ otherContexts

  } yield extents

  /**
    * Compute a topological sort of the OML Modules according to
    * the order induced by the source/target of OML ModuleEdges relations.
    *
    * @param moduleExtents A set of OML Modules & their corresponding OML Extents.
    * @param edgeExtents A set of OML ModuleEdges & their corresponding OML Extents.
    * @return The topological sort of the OML Modules & their OML Extents.
    */
  def sortExtents
  (moduleExtents: Map[api.Module, api.Extent],
   edgeExtents: Map[api.ModuleEdge, api.Extent])
  : Throwables \/ Seq[(api.Module, api.Extent)]
  = for {
    m2e <- moduleExtents.right[Throwables]

    g0 = Graph[api.Module, ModuleGraphEdge]()

    g1 = moduleExtents.foldLeft(g0) {
      case (gi, (mi, _)) =>
        gi + mi
    }

    g2 <- edgeExtents.foldLeft(g1.right[Throwables]) { case (acc, (me, ext)) =>
      for {
        gi <- acc
        source <- me.sourceModule()(ext) match {
          case Some(s) =>
            s.right[Throwables]
          case _ =>
            Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
              s"No source module for edge: $me"
            )).left
        }
        targetIRI = me.targetModule()(ext)
        gj = gi.toOuterNodes.find(_.iri == targetIRI).fold(gi) { target: api.Module =>
          val edge = new ModuleGraphEdge[api.Module](NodeProduct(source, target), me)
          gi + edge
        }
      } yield gj
    }

    g = g2

    moduleSort <-
      GraphUtilities.hierarchicalTopologicalSort[api.Module, ModuleGraphEdge](Seq(g))
        .map(_.reverse)

    result <- moduleSort.foldLeft(Seq.empty[(api.Module, api.Extent)].right[Throwables]) { case (acc, m) =>
      for {
        prev <- acc
        e <- m2e.get(m) match {
          case Some(_e) =>
            _e.right[Throwables]
          case None =>
            Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
              s"No extent for module: $m"
            )).left
        }
        next = prev :+ (m -> e)
      } yield next
    }
  } yield result

}
