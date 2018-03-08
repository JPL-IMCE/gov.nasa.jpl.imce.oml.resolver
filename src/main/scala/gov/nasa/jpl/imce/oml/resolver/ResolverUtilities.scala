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

import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.imce.oml.uuid.JVMUUIDGenerator

import scala.collection.immutable.{Map, Seq, Set}
import scala.{None, Some, StringContext, Unit}
import scala.Predef.ArrowAssoc
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
  = {
    val iri2module
    : Map[tables.taggedTypes.IRI, api.Module]
    = moduleExtents.foldLeft(Map.empty[tables.taggedTypes.IRI, api.Module]) { case (acc, (m, _)) =>
      acc + (m.iri -> m)
    }

    def nodeOp
    (gi: Graph[api.Module, ModuleGraphEdge],
     m_ext: (api.Module, api.Extent))
    : Graph[api.Module, ModuleGraphEdge]
    = gi + m_ext._1

    def edgeOp
    (acc: Throwables \/ Graph[api.Module, ModuleGraphEdge],
     me_ext: (api.ModuleEdge, api.Extent))
    : Throwables \/ Graph[api.Module, ModuleGraphEdge]
    = {
      val (me, ext) = (me_ext._1, me_ext._2)
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

        gj = iri2module.get(targetIRI).fold(gi) { target: api.Module =>
          val edge = new ModuleGraphEdge[api.Module](NodeProduct(source, target), me)
          gi + edge
        }
      } yield gj
    }

    def resultOp
    (acc: Throwables \/ (Seq[(api.Module, api.Extent)], Set[api.Module]),
     m: api.Module)
    : Throwables \/ (Seq[(api.Module, api.Extent)], Set[api.Module])
    = for {
      prev_visited <- acc
      (prev, visited) = prev_visited
      e <- moduleExtents.get(m) match {
        case Some(_e) =>
          _e.right[Throwables]
        case None =>
          Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
            s"No extent for module: $m"
          )).left
      }
      next = if (visited.contains(m)) prev else prev :+ (m -> e)
    } yield next -> (visited + m)

    val sorted = for {
      g1 <- moduleExtents.foldLeft(Graph[api.Module, ModuleGraphEdge]())(nodeOp).right[Throwables]

      g2 <- edgeExtents.foldLeft(g1.right[Throwables])(edgeOp)

      moduleSort <- GraphUtilities.hierarchicalTopologicalSort[api.Module, ModuleGraphEdge](Seq(g2)).map(_.reverse)

      result <- moduleSort.foldLeft((Seq.empty[(api.Module, api.Extent)], Set.empty[api.Module]).right[Throwables])(resultOp)
    } yield result._1

    sorted
  }

  def allModulesIncludingFrom
  (m: api.Module)
  (implicit ext: api.Extent)
  : Set[api.Module]
  = {
    val moduleByIRI
    : Map[tables.taggedTypes.IRI, api.Module]
    = ext.terminologyGraphs.values.map { m => m.iri -> m }.toMap ++
      ext.bundles.values.map { m => m.iri -> m }.toMap ++
      ext.descriptionBoxes.values.map { m => m.iri -> m }.toMap

    def step
    (fringe: Set[api.Module],
     acc: Set[api.Module])
    : Set[api.Module]
    = if (fringe.isEmpty)
      acc
    else {
      val edges = fringe.foldLeft[Set[api.ModuleEdge]](Set.empty[api.ModuleEdge]) {
        case (acc, m) =>
          acc ++ m.moduleEdges()
      }

      val next = edges.flatMap { e => moduleByIRI.get(e.targetModule()) }

      step(next, acc ++ next)
    }

    step(Set(m), Set(m))
  }

  def allTerminologyBoxesIncludingFrom
  (m: api.Module)
  (implicit ext: api.Extent)
  : Set[api.TerminologyBox]
  = {
    val moduleByIRI
    : Map[tables.taggedTypes.IRI, api.TerminologyBox]
    = ext.terminologyGraphs.values.map { m => m.iri -> m }.toMap ++
      ext.bundles.values.map { m => m.iri -> m }.toMap

    def step
    (fringe: Set[api.TerminologyBox],
     acc: Set[api.TerminologyBox])
    : Set[api.TerminologyBox]
    = if (fringe.isEmpty)
      acc
    else {
      val edges = fringe.foldLeft[Set[api.ModuleEdge]](Set.empty[api.ModuleEdge]) {
        case (acc, m) =>
          acc ++ m.moduleEdges()
      }

      val next = edges.flatMap { e => moduleByIRI.get(e.targetModule()) }

      step(next, acc ++ next)
    }

    m match {
      case tbox: api.TerminologyBox =>
        step(Set(tbox), Set(tbox))
      case _ =>
        Set.empty
    }

  }

  def rootReifiedRelationships
  (pr: api.ReifiedRelationshipRestriction)
  (implicit ext: api.Extent)
  : Set[api.ReifiedRelationship]
  = ext.terminologyBoxOfTerminologyBoxStatement.get(pr) match {
    case Some(tbox) =>
      import Filterable.filterable
      val allTboxes = allTerminologyBoxesIncludingFrom(tbox)
      val allSpecializationAxioms = allTboxes.flatMap { m =>
        ext
          .boxStatements
          .getOrElse(m, Set.empty)
          .selectByKindOf { case ax: api.ReifiedRelationshipSpecializationAxiom => ax }
      }
      Set.empty
    case None =>
      Set.empty
  }
}
