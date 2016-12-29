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

package gov.nasa.jpl.imce.omf.schema.resolver

import java.util.UUID

import gov.nasa.jpl.imce.omf.schema._

import scalax.collection.immutable.Graph
import scala.collection.immutable.{Map, Seq, Set}
import scala.util.{Success, Try}
import scala.Predef.ArrowAssoc
import scala.collection.parallel.immutable.ParSeq

case class OMFSchemaResolver private[resolver]
(context: impl.TerminologyContext,
 queue: tables.OMFSchemaTables) {

}

object OMFSchemaResolver {

  def resolve(t: tables.OMFSchemaTables)
  : Try[OMFSchemaResolver]
  = for {
    init <- Try.apply(OMFSchemaResolver(impl.TerminologyContext(), t))
    // Terminologies
    step1 <- mapTerminologyGraphs(init)
    step2 <- mapBundles(step1)
    // Atomic terms
    step3 <- mapAspects(step2)
    step4 <- mapConcepts(step3)
    step5 <- mapScalars(step4)
    step6 <- mapStructures(step5)
    // TerminologyAxiom relationships
    step7 <- mapTerminologyExtends(step6)
    step8 <- mapTerminologyNestings(step7)
    // Relational terms
    step9 <- mapRestrictedDataRanges(step8)
    // - all 4 DataRelationships
    // (EntityScalarDataProperty, EntityStructuredDataProperty, ScalarDataProperty, StructuredDataProperty)
    // - all 2 EntityRelationships
    // (ReifiedRelationship, UnreifiedRelationship)
    // Axioms
    // - ScalarOneOfLiteral
    // - TermAxioms
    // -- EntityRestrictionAxioms
    // --- Entity{Existential,Universal}RestrictionAxiom
    // -- EntityScalarDataPropertyRestrictionAxioms
    // --- EntityScalarDataProperty{Existential,Particular,Universal}RestrictionAxiom
    // -- ScalarRangeAxiom
    // --- ScalarRestrictionAxioms
    // ---- {Binary,IRI,Numeric,Plain,ScalarOneOf,String,Time}ScalarRestrictionAxiom
    // -- SpecializationAxiom
    // --- {Aspect,Concept,EntityScalarDataPropertyExistential,EntityScalarDataPropertyUniversal,ReifiedRelationship}SpecializationAxiom
  } yield step9

  def mapTerminologyGraphs
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val gN = resolver.queue.terminologyGraphs.foldLeft(resolver.context.g) { (gi, t) =>
      gi + impl.TerminologyGraph(java.util.UUID.fromString(t.uuid), t.kind, t.name, t.iri, boxStatements=Set.empty)
    }

    val r = resolver.copy(
      context = resolver.context.copy(g = gN),
      queue = resolver.queue.copy(terminologyGraphs = Seq()))
    Success(r)
  }

  def mapBundles
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val gN = resolver.queue.bundles.foldLeft(resolver.context.g) { (gi, b) =>
      gi + impl.Bundle(java.util.UUID.fromString(b.uuid), b.kind, b.name, b.iri,
        boxStatements=Set.empty,
        bundleStatements=Set.empty,
        terminologyBundleAxioms=Set.empty)
    }

    val r = resolver.copy(
      context = resolver.context.copy(g = gN),
      queue = resolver.queue.copy(bundles = Seq()))
    Success(r)
  }

  def seqopAppend[T]
  (s: Seq[T], entry: (UUID, ParSeq[T]))
  : Seq[T]
  = s ++ entry._2

  def seqopAppend1[T]
  (s: Seq[T], entry: ((UUID, UUID), T))
  : Seq[T]
  = s :+ entry._2

  def combopGraphs
  (g1: Graph[api.TerminologyBox, impl.TerminologyEdge],
   g2: Graph[api.TerminologyBox, impl.TerminologyEdge])
  : Graph[api.TerminologyBox, impl.TerminologyEdge]
  = g1.union(g2)

  def seqopAspects
  (nodes: Map[UUID, api.TerminologyBox])
  (g: Graph[api.TerminologyBox, impl.TerminologyEdge], entry: (UUID, ParSeq[tables.Aspect]))
  : Graph[api.TerminologyBox, impl.TerminologyEdge]
  = {
    val n0 = nodes(entry._1)
    val s = entry._2
      .map { e =>
        impl.Aspect(uuid=UUID.fromString(e.uuid), name=e.name, iri=e.iri)
      }
      .toSet.seq
    val result = g - n0 + n0.withBoxStatements(s)
    result
  }

  def mapAspects
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val byUUID =
      resolver.queue.aspects.par
        .groupBy(_.graphUUID)
        .map { case (uuid, aspects) => UUID.fromString(uuid) -> aspects }

    val (resolvable, unresolvable) =
      byUUID
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    val g = resolvable.aggregate(resolver.context.g)(seqop=seqopAspects(ns), combop=combopGraphs)

    val a = unresolvable.flatMap(_._2).toSeq.seq

    val r =
      resolver
        .copy(
          context = impl.TerminologyContext(g),
          queue = resolver.queue.copy(aspects=a))

    Success(r)
  }

  def seqopConcepts
  (nodes: Map[UUID, api.TerminologyBox])
  (g: Graph[api.TerminologyBox, impl.TerminologyEdge], entry: (UUID, ParSeq[tables.Concept]))
  : Graph[api.TerminologyBox, impl.TerminologyEdge]
  = {
    val n0 = nodes(entry._1)
    val s = entry._2
      .map { e =>
        impl.Concept(uuid=UUID.fromString(e.uuid), isAbstract=e.isAbstract, name=e.name, iri=e.iri)
      }
      .toSet.seq
    val result = g - n0 + n0.withBoxStatements(s)
    result
  }

  def mapConcepts
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val byUUID =
      resolver.queue.concepts.par
        .groupBy(_.graphUUID)
        .map { case (uuid, concepts) => UUID.fromString(uuid) -> concepts }

    val (resolvable, unresolvable) =
      byUUID
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    val g = resolvable.aggregate(resolver.context.g)(seqop=seqopConcepts(ns), combop=combopGraphs)

    val c = unresolvable.flatMap(_._2).toSeq.seq

    val r =
      resolver
        .copy(
          context = impl.TerminologyContext(g),
          queue = resolver.queue.copy(concepts=c))

    Success(r)
  }

  def seqopScalars
  (nodes: Map[UUID, api.TerminologyBox])
  (g: Graph[api.TerminologyBox, impl.TerminologyEdge], entry: (UUID, ParSeq[tables.Scalar]))
  : Graph[api.TerminologyBox, impl.TerminologyEdge]
  = {
    val n0 = nodes(entry._1)
    val s = entry._2
      .map { e =>
        impl.Scalar(uuid=UUID.fromString(e.uuid), name=e.name, iri=e.iri)
      }
      .toSet.seq
    val result = g - n0 + n0.withBoxStatements(s)
    result
  }

  def mapScalars
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val byUUID =
      resolver.queue.scalars.par
        .groupBy(_.graphUUID)
        .map { case (uuid, scalars) => UUID.fromString(uuid) -> scalars }

    val (resolvable, unresolvable) =
      byUUID
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    val g = resolvable.aggregate(resolver.context.g)(seqop=seqopScalars(ns), combop=combopGraphs)

    val s = unresolvable.flatMap(_._2).toSeq.seq

    val r =
      resolver
        .copy(
          context = impl.TerminologyContext(g),
          queue = resolver.queue.copy(scalars=s))

    Success(r)
  }

  def seqopStructures
  (nodes: Map[UUID, api.TerminologyBox])
  (g: Graph[api.TerminologyBox, impl.TerminologyEdge], entry: (UUID, ParSeq[tables.Structure]))
  : Graph[api.TerminologyBox, impl.TerminologyEdge]
  = {
    val n0 = nodes(entry._1)
    val s = entry._2
      .map { e =>
        impl.Structure(uuid=UUID.fromString(e.uuid), name=e.name, iri=e.iri)
      }
      .toSet.seq
    val result = g - n0 + n0.withBoxStatements(s)
    result
  }

  def mapStructures
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val byUUID =
      resolver.queue.structures.par
        .groupBy(_.graphUUID)
        .map { case (uuid, structures) => UUID.fromString(uuid) -> structures }

    val (resolvable, unresolvable) =
      byUUID
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    val g = resolvable.aggregate(resolver.context.g)(seqop=seqopStructures(ns), combop=combopGraphs)

    val s = unresolvable.flatMap(_._2).toSeq.seq

    val r =
      resolver
        .copy(
          context = impl.TerminologyContext(g),
          queue = resolver.queue.copy(structures=s))

    Success(r)
  }

  def seqopTerminologyExtends
  (nodes: Map[UUID, api.TerminologyBox])
  (g: Graph[api.TerminologyBox, impl.TerminologyEdge],
   entry: ((UUID, UUID), tables.TerminologyExtensionAxiom))
  : Graph[api.TerminologyBox, impl.TerminologyEdge]
  = {
    val extending = nodes(entry._1._1)
    val extended = nodes(entry._1._2)

    val result = g + impl.TerminologyEdge(
      extending, extended,
      impl.TerminologyExtensionAxiom(
        uuid = UUID.fromString(entry._2.uuid),
        extendingTerminology = extending,
        extendedTerminology = extended))

    result
  }

  def mapTerminologyExtends
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val byUUID =
      resolver.queue.terminologyExtensionAxioms.par
        .map { tAxiom =>
          (UUID.fromString(tAxiom.extendingTerminologyUUID), UUID.fromString(tAxiom.extendedTerminologyUUID)) -> tAxiom
        }

    val (resolvable, unresolvable) =
      byUUID
        .partition { case (graphUUIDPair, _) =>
          ns.contains(graphUUIDPair._1) && ns.contains(graphUUIDPair._2)
        }

    val g = resolvable.aggregate(resolver.context.g)(seqop=seqopTerminologyExtends(ns), combop=combopGraphs)

    val s = unresolvable.map(_._2).seq

    val r =
      resolver
        .copy(
          context = impl.TerminologyContext(g),
          queue = resolver.queue.copy(terminologyExtensionAxioms=s))

    Success(r)
  }

  def seqopTerminologyNesting
  (tc: api.TerminologyContext)
  (g: Graph[api.TerminologyBox, impl.TerminologyEdge],
   entry: ((UUID, UUID), tables.TerminologyNestingAxiom))
  : Graph[api.TerminologyBox, impl.TerminologyEdge]
  = {
    val nesting = tc.nodes(entry._1._1)
    val nested = tc.graphs(entry._1._2)

    val result = g + impl.TerminologyEdge(
      nesting, nested,
      impl.TerminologyNestingAxiom(
        uuid = UUID.fromString(entry._2.uuid),
        nestingTerminology = nesting,
        nestingContext = null, // TODO
        nestedTerminology = nested))
    result
  }

  def mapTerminologyNestings
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val byUUID =
      resolver.queue.terminologyNestingAxioms.par
        .map { tAxiom =>
          (UUID.fromString(tAxiom.nestingTerminologyUUID), UUID.fromString(tAxiom.nestedTerminologyUUID)) -> tAxiom
        }

    val (resolvable, unresolvable) =
      byUUID
        .partition { case (graphUUIDPair, _) =>
          ns.contains(graphUUIDPair._1) && ns.contains(graphUUIDPair._2)
        }

    val g =
      resolvable
      .aggregate(resolver.context.g)(seqop=seqopTerminologyNesting(resolver.context), combop=combopGraphs)

    val s =
      unresolvable.map(_._2).seq

    val r =
      resolver
        .copy(
          context = impl.TerminologyContext(g),
          queue = resolver.queue.copy(terminologyNestingAxioms=s))

    Success(r)
  }

  def mapRestrictedDataRanges
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val (r1, u1) =
      resolver.queue.binaryScalarRestrictions
        .groupBy(_.graphUUID)
        .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }
    val (r2, u2) =
        resolver.queue.iriScalarRestrictions
          .groupBy(_.graphUUID)
          .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
          .partition { case (graphUUID, _) => ns.contains(graphUUID) }
    val (r3, u3) =
        resolver.queue.numericScalarRestrictions
          .groupBy(_.graphUUID)
          .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
          .partition { case (graphUUID, _) => ns.contains(graphUUID) }
    val (r4, u4) =
        resolver.queue.plainLiteralScalarRestrictions
          .groupBy(_.graphUUID)
          .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
          .partition { case (graphUUID, _) => ns.contains(graphUUID) }
    val (r5, u5) =
        resolver.queue.scalarOneOfRestrictions
          .groupBy(_.graphUUID)
          .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
          .partition { case (graphUUID, _) => ns.contains(graphUUID) }
    val (r6, u6) =
        resolver.queue.timeScalarRestrictions
          .groupBy(_.graphUUID)
          .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
          .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    Success(resolver)
  }
}
