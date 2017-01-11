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
import scala.collection.parallel.immutable.ParSeq
import scala.{Boolean, None, Some, StringContext, Tuple2, Tuple3}
import scala.util.{Failure, Success, Try}
import scala.Predef.{require,ArrowAssoc}

case class OMFSchemaResolver private[resolver]
(context: impl.TerminologyContext,
 queue: tables.OMFSchemaTables)

object OMFSchemaResolver {

  def resolve(t: tables.OMFSchemaTables)
  : Try[OMFSchemaResolver]
  = for {
    init <- Try.apply(OMFSchemaResolver(
      impl.TerminologyContext(t.annotationProperties.map { ap =>
        UUID.fromString(ap.uuid) -> impl.AnnotationProperty(UUID.fromString(ap.uuid), ap.iri) }.toMap),
      t))
    // Terminologies
    step0a <- mapTerminologyGraphs(init)
    step0b<- mapBundles(step0a)
    // Atomic terms
    step1a <- mapAspects(step0b)
    step1b <- mapConcepts(step1a)
    step1c <- mapScalars(step1b)
    step1d <- mapStructures(step1c)
    // TerminologyBoxAxiom relationships
    step2a <- mapTerminologyExtends(step1d)
    step2b <- mapTerminologyNestings(step2a)
    step2c <- mapConceptDesignationTerminologyAxioms(step2b)
    // TerminologyBundleAxiom relationships
    step3a <- mapBundledTerminologyAxioms(step2c)
    // Relational terms
    step4a <- mapRestrictedDataRanges(step3a)
    step4b <- mapReifiedRelationships(step4a)
    step4c <- mapUnreifiedRelationships(step4b)
    // DataRelationships
    step5a <- mapEntityScalarDataProperties(step4c)
    step5b <- mapEntityStructuredDataProperties(step5a)
    step5c <- mapScalarDataProperties(step5b)
    step5d <- mapStructuredDataProperties(step5c)
    // Axioms
    step6a <- mapScalarOneOfLiteralAxioms(step5d)
    // - TermAxioms
    // -- EntityRestrictionAxioms
    step7a <- mapEntityExistentialRestrictionAxioms(step6a)
    step7b <- mapEntityUniversalRestrictionAxioms(step7a)
    // -- EntityScalarDataPropertyRestrictionAxioms
    step8a <- mapEntityScalarDataPropertyExistentialRestrictionAxioms(step7b)
    step8b <- mapEntityScalarDataPropertyParticularRestrictionAxioms(step8a)
    step8c <- mapEntityScalarDataPropertyUniversalRestrictionAxioms(step8b)
    // -- SpecializationAxiom
    step9a <- mapAspectSpecializationAxioms(step8c)
    step9b <- mapConceptSpecializationAxioms(step9a)
    step9c <- mapReifiedRelationshipSpecializationAxioms(step9b)
    // TerminologyBundleStatements
    step10a <- mapRootConceptTaxonomyAxioms(step9c)
    step10b <- mapAnonymousConceptTaxonomyAxioms(step10a)
    step10c <- mapSpecificDisjointConceptAxioms(step10b)
    // AnnotationPairs
    step11 <- mapAnnotationPairs(step10c)
  } yield step11

  def mapTerminologyGraphs
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val gN = resolver.queue.terminologyGraphs.foldLeft(resolver.context.g) { (gi, t) =>
      gi + impl.TerminologyGraph(
        java.util.UUID.fromString(t.uuid), t.kind, t.name, t.iri,
        annotations=Map.empty,
        boxStatements=Set.empty)
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
        annotations=Map.empty,
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

  type HyperGraphV = Try[Graph[api.TerminologyBox, impl.TerminologyEdge]]

  def seqopAspects
  (h: HyperGraphV, entry: (UUID, Seq[tables.Aspect]))
  : HyperGraphV
  = h.flatMap { g =>
    g
      .toOuterNodes
      .find(_.uuid == entry._1)
      .fold[HyperGraphV](
      Failure(new java.lang.Error(
        s"OMFSchemaResolver.seqopAspects: there should be a graph with UUID=${entry._1}"))
    ) { n0 =>

      val s = entry._2
        .map { e =>
          impl.Aspect(uuid = UUID.fromString(e.uuid), name = e.name, iri = e.iri)
        }
        .toSet.seq
      val result = impl.TerminologyContext.replaceNode(g, n0, n0.withBoxStatements(s))
      result
    }
  }

  def mapAspects
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val byUUID =
      resolver.queue.aspects
        .groupBy(_.graphUUID)
        .map { case (uuid, aspects) => UUID.fromString(uuid) -> aspects }

    val (resolvable, unresolvable) =
      byUUID
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    resolvable
      .foldLeft[HyperGraphV](Success(resolver.context.g))(seqopAspects)
      .map { g =>
        resolver
          .copy(
            context = impl.TerminologyContext(resolver.context.annotationProperties, g),
            queue = resolver.queue.copy(aspects = unresolvable.flatMap(_._2).to[Seq]))
      }
  }

  def seqopConcepts
  (h: HyperGraphV, entry: (UUID, Seq[tables.Concept]))
  : HyperGraphV
  = h.flatMap { g =>
    g
      .toOuterNodes
      .find(_.uuid == entry._1)
      .fold[HyperGraphV](
      Failure(new java.lang.Error(
        s"OMFSchemaResolver.seqopConcepts: there should be a graph with UUID=${entry._1}"))
    ) { n0 =>

      val s = entry._2
        .map { e =>
          impl.Concept(uuid = UUID.fromString(e.uuid), isAbstract = e.isAbstract, name = e.name, iri = e.iri)
        }
        .toSet.seq
      val result = impl.TerminologyContext.replaceNode(g, n0, n0.withBoxStatements(s))
      result
    }
  }

  def mapConcepts
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val byUUID =
      resolver.queue.concepts
        .groupBy(_.graphUUID)
        .map { case (uuid, concepts) => UUID.fromString(uuid) -> concepts }

    val (resolvable, unresolvable) =
      byUUID
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    resolvable
      .foldLeft[HyperGraphV](Success(resolver.context.g))(seqopConcepts)
      .map { g =>
        resolver
          .copy(
            context = impl.TerminologyContext(resolver.context.annotationProperties, g),
            queue = resolver.queue.copy(concepts = unresolvable.flatMap(_._2).to[Seq]))
      }
  }

  def seqopScalars
  (h: HyperGraphV, entry: (UUID, Seq[tables.Scalar]))
  : HyperGraphV
  = h.flatMap { g =>
    g
      .toOuterNodes
      .find(_.uuid == entry._1)
      .fold[HyperGraphV](
      Failure(new java.lang.Error(
        s"OMFSchemaResolver.seqopScalars: there should be a graph with UUID=${entry._1}"))
    ) { n0 =>

      val s = entry._2
        .map { e =>
          impl.Scalar(uuid = UUID.fromString(e.uuid), name = e.name, iri = e.iri)
        }
        .toSet.seq
      val result = impl.TerminologyContext.replaceNode(g, n0, n0.withBoxStatements(s))
      result
    }
  }

  def mapScalars
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val byUUID =
      resolver.queue.scalars
        .groupBy(_.graphUUID)
        .map { case (uuid, scalars) => UUID.fromString(uuid) -> scalars }

    val (resolvable, unresolvable) =
      byUUID
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    resolvable
      .foldLeft[HyperGraphV](Success(resolver.context.g))(seqopScalars)
      .map { g =>
        resolver
          .copy(
            context = impl.TerminologyContext(resolver.context.annotationProperties, g),
            queue = resolver.queue.copy(scalars = unresolvable.flatMap(_._2).to[Seq]))
      }
  }

  def seqopStructures
  (h: HyperGraphV, entry: (UUID, Seq[tables.Structure]))
  : HyperGraphV
  = h.flatMap { g =>
    g
      .toOuterNodes
      .find(_.uuid == entry._1)
      .fold[HyperGraphV](
      Failure(new java.lang.Error(
        s"OMFSchemaResolver.seqopStructures: there should be a graph with UUID=${entry._1}"))
    ) { n0 =>
      val s = entry._2
        .map { e =>
          impl.Structure(uuid = UUID.fromString(e.uuid), name = e.name, iri = e.iri)
        }
        .toSet.seq
      val result = impl.TerminologyContext.replaceNode(g, n0, n0.withBoxStatements(s))
      result
    }
  }

  def mapStructures
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val byUUID =
      resolver.queue.structures
        .groupBy(_.graphUUID)
        .map { case (uuid, structures) => UUID.fromString(uuid) -> structures }

    val (resolvable, unresolvable) =
      byUUID
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    resolvable
      .foldLeft[HyperGraphV](Success(resolver.context.g))(seqopStructures)
      .map { g =>
        resolver
          .copy(
            context = impl.TerminologyContext(resolver.context.annotationProperties, g),
            queue = resolver.queue.copy(structures = unresolvable.flatMap(_._2).to[Seq]))
      }
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

    val g = resolvable.aggregate(resolver.context.g)(seqop = seqopTerminologyExtends(ns), combop = combopGraphs)

    val s = unresolvable.map(_._2).seq

    val r =
      resolver
        .copy(
          context = impl.TerminologyContext(resolver.context.annotationProperties, g),
          queue = resolver.queue.copy(terminologyExtensionAxioms = s))

    Success(r)
  }

  def seqopTerminologyNesting
  (ns: Map[UUID, api.TerminologyGraph],
   cs: Map[UUID, (api.Concept, api.TerminologyBox)])
  (g: Graph[api.TerminologyBox, impl.TerminologyEdge],
   entry: ((UUID, UUID), tables.TerminologyNestingAxiom))
  : Graph[api.TerminologyBox, impl.TerminologyEdge]
  = {
    val nestedT = ns(entry._1._1)
    val (nestingC, nestingT) = cs(entry._1._2)

    val result = g + impl.TerminologyEdge(
      nestedT, nestingT,
      impl.TerminologyNestingAxiom(
        uuid = UUID.fromString(entry._2.uuid),
        nestedTerminology = nestedT,
        nestingContext = nestingC,
        nestingTerminology = nestingT))
    result
  }

  def mapTerminologyNestings
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes.flatMap {
      case (uuid, g: api.TerminologyGraph) =>
        Some(uuid -> g)
      case _ =>
        None
    }
    val cs = resolver.context.g.nodes.toOuter.flatMap { t =>
      t.concepts.map { case (cUUID, c) =>
        cUUID -> (c -> t)
      }
    }.toMap

    val byUUID =
      resolver.queue.terminologyNestingAxioms.par
        .map { tAxiom =>
          (UUID.fromString(tAxiom.nestedTerminologyUUID), UUID.fromString(tAxiom.nestingContextUUID)) -> tAxiom
        }

    val (resolvable, unresolvable) =
      byUUID
        .partition { case (graphUUIDPair, tx) =>
          ns.contains(graphUUIDPair._1) &&
            cs.get(graphUUIDPair._2).fold[Boolean](false){ case (c,tbox) =>
                tbox.uuid == UUID.fromString(tx.nestingTerminologyUUID)
            }
        }

    val g =
      resolvable
        .aggregate(resolver.context.g)(seqop = seqopTerminologyNesting(ns, cs), combop = combopGraphs)

    val s =
      unresolvable.map(_._2).seq

    val r =
      resolver
        .copy(
          context = impl.TerminologyContext(resolver.context.annotationProperties, g),
          queue = resolver.queue.copy(terminologyNestingAxioms = s))

    Success(r)
  }

  def seqopConceptDesignationTerminology
  (ns: Map[UUID, api.TerminologyGraph],
   cs: Map[UUID, (api.Concept, api.TerminologyBox)])
  (g: Graph[api.TerminologyBox, impl.TerminologyEdge],
   entry: ((UUID, UUID), tables.ConceptDesignationTerminologyAxiom))
  : Graph[api.TerminologyBox, impl.TerminologyEdge]
  = {
    val designationG = ns(entry._1._1)
    val (designatedC, designatedTBox) = cs(entry._1._2)

    val result = g + impl.TerminologyEdge(
      designationG, designatedTBox,
      impl.ConceptDesignationTerminologyAxiom(
        uuid = UUID.fromString(entry._2.uuid),
        designatedConcept = designatedC,
        designatedTerminology = designatedTBox,
        designationTerminologyGraph = designationG))

    result
  }

  def mapConceptDesignationTerminologyAxioms
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes.flatMap {
      case (uuid, g: api.TerminologyGraph) =>
        Some(uuid -> g)
      case _ =>
        None
    }
    val cs = resolver.context.g.nodes.toOuter.flatMap { t =>
      t.concepts.map { case (cUUID, c) =>
          cUUID -> (c -> t)
      }
    }.toMap

    val byUUID =
      resolver.queue.conceptDesignationTerminologyAxioms.par
        .map { tAxiom =>
          (UUID.fromString(tAxiom.designationTerminologyGraphUUID), UUID.fromString(tAxiom.designatedConceptUUID)) -> tAxiom
        }

    val (resolvable, unresolvable) =
      byUUID
        .partition { case (graphUUIDPair, _) =>
          ns.contains(graphUUIDPair._1) &&
            cs.contains(graphUUIDPair._2)
        }

    val g =
      resolvable
        .aggregate(resolver.context.g)(seqop = seqopConceptDesignationTerminology(ns, cs), combop = combopGraphs)

    val s =
      unresolvable.map(_._2).seq

    val r =
      resolver
        .copy(
          context = impl.TerminologyContext(resolver.context.annotationProperties, g),
          queue = resolver.queue.copy(conceptDesignationTerminologyAxioms = s))

    Success(r)
  }

  def seqopBundledTerminologyAxioms
  (bundles: Map[UUID, api.Bundle],
   nodes: Map[UUID, api.TerminologyBox])
  (g: Graph[api.TerminologyBox, impl.TerminologyEdge],
   entry: ((UUID, UUID), tables.BundledTerminologyAxiom))
  : Graph[api.TerminologyBox, impl.TerminologyEdge]
  = {
    val bundling = bundles(entry._1._1)
    val bundled = nodes(entry._1._2)

    val result = g + impl.TerminologyEdge(
      bundling, bundled,
      impl.BundledTerminologyAxiom(
        uuid = UUID.fromString(entry._2.uuid),
        terminologyBundle = bundling,
        bundledTerminology = bundled))

    result
  }

  def mapBundledTerminologyAxioms
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val bs = resolver.context.bundles
    val ns = resolver.context.nodes
    val byUUID =
      resolver.queue.bundledTerminologyAxioms.par
        .map { tAxiom =>
          (UUID.fromString(tAxiom.terminologyBundleUUID), UUID.fromString(tAxiom.bundledTerminologyUUID)) -> tAxiom
        }

    val (resolvable, unresolvable) =
      byUUID
        .partition { case (graphUUIDPair, _) =>
          bs.contains(graphUUIDPair._1) && ns.contains(graphUUIDPair._2)
        }

    val g = resolvable.aggregate(resolver.context.g)(seqop = seqopBundledTerminologyAxioms(bs, ns), combop = combopGraphs)

    val s = unresolvable.map(_._2).seq

    val r =
      resolver
        .copy(
          context = impl.TerminologyContext(resolver.context.annotationProperties, g),
          queue = resolver.queue.copy(bundledTerminologyAxioms = s))

    Success(r)
  }

  def mapRestrictedDataRanges
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val g = resolver.context.g
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
      resolver.queue.stringScalarRestrictions
        .groupBy(_.graphUUID)
        .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }
    val (r7, u7) =
      resolver.queue.timeScalarRestrictions
        .groupBy(_.graphUUID)
        .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    val worklist = DataRangesToResolve(
      binaryScalarRestrictions =
        r1.map { case (guuid, drs) => guuid -> drs.map(dr => UUID.fromString(dr.restrictedRangeUUID) -> dr) },
      iriScalarRestrictions =
        r2.map { case (guuid, drs) => guuid -> drs.map(dr => UUID.fromString(dr.restrictedRangeUUID) -> dr) },
      numericScalarRestrictions =
        r3.map { case (guuid, drs) => guuid -> drs.map(dr => UUID.fromString(dr.restrictedRangeUUID) -> dr) },
      plainLiteralScalarRestrictions =
        r4.map { case (guuid, drs) => guuid -> drs.map(dr => UUID.fromString(dr.restrictedRangeUUID) -> dr) },
      scalarOneOfRestrictions =
        r5.map { case (guuid, drs) => guuid -> drs.map(dr => UUID.fromString(dr.restrictedRangeUUID) -> dr) },
      stringScalarRestrictions =
        r6.map { case (guuid, drs) => guuid -> drs.map(dr => UUID.fromString(dr.restrictedRangeUUID) -> dr) },
      timeScalarRestrictions =
        r7.map { case (guuid, drs) => guuid -> drs.map(dr => UUID.fromString(dr.restrictedRangeUUID) -> dr) }
    )

    DataRangesToResolve
      .resolve(resolver, worklist)
      .map { case (resolved, remaining) =>

        val updated = resolved.queue.copy(
          binaryScalarRestrictions =
            u1.flatMap(_._2).to[Seq] ++ remaining.binaryScalarRestrictions.flatMap(_._2.map(_._2)).to[Seq],
          iriScalarRestrictions =
            u2.flatMap(_._2).to[Seq] ++ remaining.iriScalarRestrictions.flatMap(_._2.map(_._2)).to[Seq],
          numericScalarRestrictions =
            u3.flatMap(_._2).to[Seq] ++ remaining.numericScalarRestrictions.flatMap(_._2.map(_._2)).to[Seq],
          plainLiteralScalarRestrictions =
            u4.flatMap(_._2).to[Seq] ++ remaining.plainLiteralScalarRestrictions.flatMap(_._2.map(_._2)).to[Seq],
          scalarOneOfRestrictions =
            u5.flatMap(_._2).to[Seq] ++ remaining.scalarOneOfRestrictions.flatMap(_._2.map(_._2)).to[Seq],
          stringScalarRestrictions =
            u6.flatMap(_._2).to[Seq] ++ remaining.stringScalarRestrictions.flatMap(_._2.map(_._2)).to[Seq],
          timeScalarRestrictions =
            u7.flatMap(_._2).to[Seq] ++ remaining.timeScalarRestrictions.flatMap(_._2.map(_._2)).to[Seq]
        )

        resolved.copy(queue = updated)
      }
  }

  def mapReifiedRelationships
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val g = resolver.context.g
    val (resolvable, unresolved) =
      resolver.queue.reifiedRelationships
        .groupBy(_.graphUUID)
        .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    val r1 = resolver.copy(queue = resolver.queue.copy(reifiedRelationships = unresolved.flatMap(_._2).to[Seq]))
    resolveReifiedRelationships(r1, resolvable).map {
      case (r2, remaining) =>
        r2.copy(queue = r2.queue.copy(reifiedRelationships = r2.queue.reifiedRelationships ++ remaining))
    }
  }

  final def resolveReifiedRelationships
  (resolver: OMFSchemaResolver, queue: Map[UUID, Seq[tables.ReifiedRelationship]])
  : Try[(OMFSchemaResolver, Seq[tables.ReifiedRelationship])]
  = {
    queue
      .foldLeft[Try[(OMFSchemaResolver, Map[UUID, Seq[tables.ReifiedRelationship]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)
        val accessible = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.entities).toMap
        val (available, remaining) = x.partition { rr =>
          accessible.contains(UUID.fromString(rr.sourceUUID)) &&
            accessible.contains(UUID.fromString(rr.targetUUID))
        }
        val si = available
          .map { rr =>
            impl.ReifiedRelationship(
              UUID.fromString(rr.uuid),
              rr.isAbstract,
              rr.name,
              rr.unreifiedPropertyName,
              rr.unreifiedInversePropertyName,
              rr.iri,
              rr.isAsymmetric,
              rr.isEssential,
              rr.isFunctional,
              rr.isInverseEssential,
              rr.isInverseFunctional,
              rr.isIrreflexive,
              rr.isReflexive,
              rr.isSymmetric,
              rr.isTransitive,
              accessible(UUID.fromString(rr.sourceUUID)),
              accessible(UUID.fromString(rr.targetUUID)))
          }
          .to[Set]
        impl.TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = impl.TerminologyContext(ri.context.annotationProperties, gj)),
              mi + (guuid -> remaining),
              fi || si.nonEmpty)
          }
      case (Failure(t), _) =>
        Failure(t)
    } match {
      case Success(Tuple3(r, m, f)) =>
        if (f)
          resolveReifiedRelationships(r, m)
        else
          Success(Tuple2(r, m.flatMap(_._2).to[Seq]))
      case Failure(t) =>
        Failure(t)
    }
  }

  def mapUnreifiedRelationships
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val g = resolver.context.g
    val (resolvable, unresolved) =
      resolver.queue.unreifiedRelationships
        .groupBy(_.graphUUID)
        .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    val r1 = resolver.copy(queue = resolver.queue.copy(unreifiedRelationships = unresolved.flatMap(_._2).to[Seq]))
    resolveUnreifiedRelationships(r1, resolvable).map {
      case (r2, remaining) =>
        r2.copy(queue = r2.queue.copy(unreifiedRelationships = r2.queue.unreifiedRelationships ++ remaining))
    }
  }

  final def resolveUnreifiedRelationships
  ( resolver: OMFSchemaResolver, queue: Map[UUID, Seq[tables.UnreifiedRelationship]] )
  : Try[(OMFSchemaResolver, Seq[tables.UnreifiedRelationship])]
  = {
    queue
      .foldLeft[Try[(OMFSchemaResolver, Map[UUID, Seq[tables.UnreifiedRelationship]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)
        val accessible = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.entities).toMap
        val (available, remaining) = x.partition { rr =>
          accessible.contains(UUID.fromString(rr.sourceUUID)) &&
            accessible.contains(UUID.fromString(rr.targetUUID))
        }
        val si = available
          .map { ur =>
            impl.UnreifiedRelationship(
              UUID.fromString(ur.uuid),
              ur.name,
              ur.iri,
              ur.isAsymmetric,
              ur.isEssential,
              ur.isFunctional,
              ur.isInverseEssential,
              ur.isInverseFunctional,
              ur.isIrreflexive,
              ur.isReflexive,
              ur.isSymmetric,
              ur.isTransitive,
              accessible(UUID.fromString(ur.sourceUUID)),
              accessible(UUID.fromString(ur.targetUUID)))
          }
          .to[Set]
        impl.TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = impl.TerminologyContext(ri.context.annotationProperties, gj)),
              mi + (guuid -> remaining),
              fi || si.nonEmpty)
          }
      case (Failure(t), _) =>
        Failure(t)
    } match {
      case Success(Tuple3(r, m, f)) =>
        if (f)
          resolveUnreifiedRelationships(r, m)
        else
          Success(Tuple2(r, m.flatMap(_._2).to[Seq]))
      case Failure(t) =>
        Failure(t)
    }
  }

  def mapEntityScalarDataProperties
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val g = resolver.context.g
    val (resolvable, unresolved) =
      resolver.queue.entityScalarDataProperties
        .groupBy(_.graphUUID)
        .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    val r1 = resolver.copy(queue = resolver.queue.copy(entityScalarDataProperties = unresolved.flatMap(_._2).to[Seq]))
    resolveEntityScalarDataProperties(r1, resolvable).map {
      case (r2, remaining) =>
        r2.copy(queue = r2.queue.copy(entityScalarDataProperties = r2.queue.entityScalarDataProperties ++ remaining))
    }
  }

  final def resolveEntityScalarDataProperties
  ( resolver: OMFSchemaResolver, queue: Map[UUID, Seq[tables.EntityScalarDataProperty]] )
  : Try[(OMFSchemaResolver, Seq[tables.EntityScalarDataProperty])]
  = {
    queue
      .foldLeft[Try[(OMFSchemaResolver, Map[UUID, Seq[tables.EntityScalarDataProperty]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)
        val entities = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.entities).toMap
        val dataranges = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.dataranges).toMap
        val (available, remaining) = x.partition { dr =>
          entities.contains(UUID.fromString(dr.domainUUID)) &&
            dataranges.contains(UUID.fromString(dr.rangeUUID))
        }
        val si = available
          .map { dr =>
            impl.EntityScalarDataProperty(
              UUID.fromString(dr.uuid),
              dr.name,
              dr.iri,
              entities(UUID.fromString(dr.domainUUID)),
              dataranges(UUID.fromString(dr.rangeUUID)))
          }
          .to[Set]
        impl.TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = impl.TerminologyContext(ri.context.annotationProperties, gj)),
              mi + (guuid -> remaining),
              fi || si.nonEmpty)
          }
      case (Failure(t), _) =>
        Failure(t)
    } match {
      case Success(Tuple3(r, m, f)) =>
        if (f)
          resolveEntityScalarDataProperties(r, m)
        else
          Success(Tuple2(r, m.flatMap(_._2).to[Seq]))
      case Failure(t) =>
        Failure(t)
    }
  }

  def mapEntityStructuredDataProperties
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val g = resolver.context.g
    val (resolvable, unresolved) =
      resolver.queue.entityStructuredDataProperties
        .groupBy(_.graphUUID)
        .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    val r1 = resolver.copy(queue = resolver.queue.copy(entityStructuredDataProperties = unresolved.flatMap(_._2).to[Seq]))
    resolveEntityStructuredDataProperties(r1, resolvable).map {
      case (r2, remaining) =>
        r2.copy(queue = r2.queue.copy(entityStructuredDataProperties = r2.queue.entityStructuredDataProperties ++ remaining))
    }
  }

  final def resolveEntityStructuredDataProperties
  ( resolver: OMFSchemaResolver, queue: Map[UUID, Seq[tables.EntityStructuredDataProperty]] )
  : Try[(OMFSchemaResolver, Seq[tables.EntityStructuredDataProperty])]
  = {
    queue
      .foldLeft[Try[(OMFSchemaResolver, Map[UUID, Seq[tables.EntityStructuredDataProperty]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)
        val entities = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.entities).toMap
        val structures = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.structures).toMap
        val (available, remaining) = x.partition { dr =>
          entities.contains(UUID.fromString(dr.domainUUID)) &&
            structures.contains(UUID.fromString(dr.rangeUUID))
        }
        val si = available
          .map { dr =>
            impl.EntityStructuredDataProperty(
              UUID.fromString(dr.uuid),
              dr.name,
              dr.iri,
              entities(UUID.fromString(dr.domainUUID)),
              structures(UUID.fromString(dr.rangeUUID)))
          }
          .to[Set]
        impl.TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = impl.TerminologyContext(ri.context.annotationProperties, gj)),
              mi + (guuid -> remaining),
              fi || si.nonEmpty)
          }
      case (Failure(t), _) =>
        Failure(t)
    } match {
      case Success(Tuple3(r, m, f)) =>
        if (f)
          resolveEntityStructuredDataProperties(r, m)
        else
          Success(Tuple2(r, m.flatMap(_._2).to[Seq]))
      case Failure(t) =>
        Failure(t)
    }
  }

  def mapScalarDataProperties
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val g = resolver.context.g
    val (resolvable, unresolved) =
      resolver.queue.scalarDataProperties
        .groupBy(_.graphUUID)
        .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    val r1 = resolver.copy(queue = resolver.queue.copy(scalarDataProperties = unresolved.flatMap(_._2).to[Seq]))
    resolveScalarDataProperties(r1, resolvable).map {
      case (r2, remaining) =>
        r2.copy(queue = r2.queue.copy(scalarDataProperties = r2.queue.scalarDataProperties ++ remaining))
    }
  }

  final def resolveScalarDataProperties
  ( resolver: OMFSchemaResolver, queue: Map[UUID, Seq[tables.ScalarDataProperty]] )
  : Try[(OMFSchemaResolver, Seq[tables.ScalarDataProperty])]
  = {
    queue
      .foldLeft[Try[(OMFSchemaResolver, Map[UUID, Seq[tables.ScalarDataProperty]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)
        val structures = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.structures).toMap
        val dataranges = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.dataranges).toMap
        val (available, remaining) = x.partition { dr =>
          structures.contains(UUID.fromString(dr.domainUUID)) &&
            dataranges.contains(UUID.fromString(dr.rangeUUID))
        }
        val si = available
          .map { dr =>
            impl.ScalarDataProperty(
              UUID.fromString(dr.uuid),
              dr.name,
              dr.iri,
              structures(UUID.fromString(dr.domainUUID)),
              dataranges(UUID.fromString(dr.rangeUUID)))
          }
          .to[Set]
        impl.TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = impl.TerminologyContext(ri.context.annotationProperties, gj)),
              mi + (guuid -> remaining),
              fi || si.nonEmpty)
          }
      case (Failure(t), _) =>
        Failure(t)
    } match {
      case Success(Tuple3(r, m, f)) =>
        if (f)
          resolveScalarDataProperties(r, m)
        else
          Success(Tuple2(r, m.flatMap(_._2).to[Seq]))
      case Failure(t) =>
        Failure(t)
    }
  }

  def mapStructuredDataProperties
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val g = resolver.context.g
    val (resolvable, unresolved) =
      resolver.queue.structuredDataProperties
        .groupBy(_.graphUUID)
        .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    val r1 = resolver.copy(queue = resolver.queue.copy(structuredDataProperties = unresolved.flatMap(_._2).to[Seq]))
    resolveStructuredDataProperties(r1, resolvable).map {
      case (r2, remaining) =>
        r2.copy(queue = r2.queue.copy(structuredDataProperties = r2.queue.structuredDataProperties ++ remaining))
    }
  }

  final def resolveStructuredDataProperties
  ( resolver: OMFSchemaResolver, queue: Map[UUID, Seq[tables.StructuredDataProperty]] )
  : Try[(OMFSchemaResolver, Seq[tables.StructuredDataProperty])]
  = {
    queue
      .foldLeft[Try[(OMFSchemaResolver, Map[UUID, Seq[tables.StructuredDataProperty]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)
        val structures = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.structures).toMap
        val (available, remaining) = x.partition { dr =>
          structures.contains(UUID.fromString(dr.domainUUID)) &&
            structures.contains(UUID.fromString(dr.rangeUUID))
        }
        val si = available
          .map { dr =>
            impl.StructuredDataProperty(
              UUID.fromString(dr.uuid),
              dr.name,
              dr.iri,
              structures(UUID.fromString(dr.domainUUID)),
              structures(UUID.fromString(dr.rangeUUID)))
          }
          .to[Set]
        impl.TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = impl.TerminologyContext(ri.context.annotationProperties, gj)),
              mi + (guuid -> remaining),
              fi || si.nonEmpty)
          }
      case (Failure(t), _) =>
        Failure(t)
    } match {
      case Success(Tuple3(r, m, f)) =>
        if (f)
          resolveStructuredDataProperties(r, m)
        else
          Success(Tuple2(r, m.flatMap(_._2).to[Seq]))
      case Failure(t) =>
        Failure(t)
    }
  }

  def mapScalarOneOfLiteralAxioms
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val g = resolver.context.g
    val (resolvable, unresolved) =
      resolver.queue.scalarOneOfLiteralAxioms
        .groupBy(_.graphUUID)
        .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    val r1 = resolver.copy(queue = resolver.queue.copy(scalarOneOfLiteralAxioms = unresolved.flatMap(_._2).to[Seq]))
    resolveScalarOneOfLiteralAxioms(r1, resolvable).map {
      case (r2, remaining) =>
        r2.copy(queue = r2.queue.copy(scalarOneOfLiteralAxioms = r2.queue.scalarOneOfLiteralAxioms ++ remaining))
    }
  }

  final def resolveScalarOneOfLiteralAxioms
  ( resolver: OMFSchemaResolver, queue: Map[UUID, Seq[tables.ScalarOneOfLiteralAxiom]] )
  : Try[(OMFSchemaResolver, Seq[tables.ScalarOneOfLiteralAxiom])]
  = {
    queue.foldLeft[Try[(OMFSchemaResolver, Map[UUID, Seq[tables.ScalarOneOfLiteralAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)
        val scalarOneOfRestrictions = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.boxStatements.flatMap {
          case ax: api.ScalarOneOfRestriction =>
            Some(ax.uuid -> ax)
          case _ =>
            None
        }).toMap
        val (available, remaining) = x.partition { ax =>
          scalarOneOfRestrictions.contains(UUID.fromString(ax.axiomUUID))
        }
        val si = available
          .map { ax =>
            impl.ScalarOneOfLiteralAxiom(
              UUID.fromString(ax.uuid),
              scalarOneOfRestrictions(UUID.fromString(ax.axiomUUID)),
              ax.value)
          }
          .to[Set]
        impl.TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = impl.TerminologyContext(ri.context.annotationProperties, gj)),
              mi + (guuid -> remaining),
              fi || si.nonEmpty)
          }
      case (Failure(t), _) =>
        Failure(t)
    } match {
      case Success(Tuple3(r, m, f)) =>
        if (f)
          resolveScalarOneOfLiteralAxioms(r, m)
        else
          Success(Tuple2(r, m.flatMap(_._2).to[Seq]))
      case Failure(t) =>
        Failure(t)
    }
  }

  def mapEntityExistentialRestrictionAxioms
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val g = resolver.context.g
    val (resolvable, unresolved) =
      resolver.queue.entityExistentialRestrictionAxioms
        .groupBy(_.graphUUID)
        .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    val r1 = resolver.copy(queue = resolver.queue.copy(entityExistentialRestrictionAxioms = unresolved.flatMap(_._2).to[Seq]))
    resolveEntityExistentialRestrictionAxioms(r1, resolvable).map {
      case (r2, remaining) =>
        r2.copy(queue = r2.queue.copy(entityExistentialRestrictionAxioms = r2.queue.entityExistentialRestrictionAxioms ++ remaining))
    }
  }

  final def resolveEntityExistentialRestrictionAxioms
  ( resolver: OMFSchemaResolver, queue: Map[UUID, Seq[tables.EntityExistentialRestrictionAxiom]] )
  : Try[(OMFSchemaResolver, Seq[tables.EntityExistentialRestrictionAxiom])]
  = {
    queue.foldLeft[Try[(OMFSchemaResolver, Map[UUID, Seq[tables.EntityExistentialRestrictionAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)
        val entities = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.entities).toMap
        val reifiedRelationships = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.reifiedRelationships).toMap
        val (available, remaining) = x.partition { ax =>
          entities.contains(UUID.fromString(ax.restrictedDomainUUID)) &&
            entities.contains(UUID.fromString(ax.restrictedRangeUUID)) &&
            reifiedRelationships.contains(UUID.fromString(ax.restrictedRelationUUID))
        }
        val si = available
          .map { ax =>
            impl.EntityExistentialRestrictionAxiom(
              UUID.fromString(ax.uuid),
              entities(UUID.fromString(ax.restrictedDomainUUID)),
              entities(UUID.fromString(ax.restrictedRangeUUID)),
              reifiedRelationships(UUID.fromString(ax.restrictedRelationUUID)))
          }
          .to[Set]
        impl.TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = impl.TerminologyContext(ri.context.annotationProperties, gj)),
              mi + (guuid -> remaining),
              fi || si.nonEmpty)
          }
      case (Failure(t), _) =>
        Failure(t)
    } match {
      case Success(Tuple3(r, m, f)) =>
        if (f)
          resolveEntityExistentialRestrictionAxioms(r, m)
        else
          Success(Tuple2(r, m.flatMap(_._2).to[Seq]))
      case Failure(t) =>
        Failure(t)
    }
  }

  def mapEntityUniversalRestrictionAxioms
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val g = resolver.context.g
    val (resolvable, unresolved) =
      resolver.queue.entityUniversalRestrictionAxioms
        .groupBy(_.graphUUID)
        .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    val r1 = resolver.copy(queue = resolver.queue.copy(entityUniversalRestrictionAxioms = unresolved.flatMap(_._2).to[Seq]))
    resolveEntityUniversalRestrictionAxioms(r1, resolvable).map {
      case (r2, remaining) =>
        r2.copy(queue = r2.queue.copy(entityUniversalRestrictionAxioms = r2.queue.entityUniversalRestrictionAxioms ++ remaining))
    }
  }

  final def resolveEntityUniversalRestrictionAxioms
  ( resolver: OMFSchemaResolver, queue: Map[UUID, Seq[tables.EntityUniversalRestrictionAxiom]] )
  : Try[(OMFSchemaResolver, Seq[tables.EntityUniversalRestrictionAxiom])]
  = {
    queue.foldLeft[Try[(OMFSchemaResolver, Map[UUID, Seq[tables.EntityUniversalRestrictionAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)
        val entities = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.entities).toMap
        val reifiedRelationships = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.reifiedRelationships).toMap
        val (available, remaining) = x.partition { ax =>
          entities.contains(UUID.fromString(ax.restrictedDomainUUID)) &&
            entities.contains(UUID.fromString(ax.restrictedRangeUUID)) &&
            reifiedRelationships.contains(UUID.fromString(ax.restrictedRelationUUID))
        }
        val si = available
          .map { ax =>
            impl.EntityUniversalRestrictionAxiom(
              UUID.fromString(ax.uuid),
              entities(UUID.fromString(ax.restrictedDomainUUID)),
              entities(UUID.fromString(ax.restrictedRangeUUID)),
              reifiedRelationships(UUID.fromString(ax.restrictedRelationUUID)))
          }
          .to[Set]
        impl.TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = impl.TerminologyContext(ri.context.annotationProperties, gj)),
              mi + (guuid -> remaining),
              fi || si.nonEmpty)
          }
      case (Failure(t), _) =>
        Failure(t)
    } match {
      case Success(Tuple3(r, m, f)) =>
        if (f)
          resolveEntityUniversalRestrictionAxioms(r, m)
        else
          Success(Tuple2(r, m.flatMap(_._2).to[Seq]))
      case Failure(t) =>
        Failure(t)
    }
  }

  def mapEntityScalarDataPropertyExistentialRestrictionAxioms
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val g = resolver.context.g
    val (resolvable, unresolved) =
      resolver.queue.entityScalarDataPropertyExistentialRestrictionAxioms
        .groupBy(_.graphUUID)
        .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    val r1 = resolver.copy(queue = resolver.queue.copy(entityScalarDataPropertyExistentialRestrictionAxioms = unresolved.flatMap(_._2).to[Seq]))
    resolveEntityScalarDataPropertyExistentialRestrictionAxioms(r1, resolvable).map {
      case (r2, remaining) =>
        r2.copy(queue = r2.queue.copy(entityScalarDataPropertyExistentialRestrictionAxioms = r2.queue.entityScalarDataPropertyExistentialRestrictionAxioms ++ remaining))
    }
  }

  final def resolveEntityScalarDataPropertyExistentialRestrictionAxioms
  ( resolver: OMFSchemaResolver, queue: Map[UUID, Seq[tables.EntityScalarDataPropertyExistentialRestrictionAxiom]] )
  : Try[(OMFSchemaResolver, Seq[tables.EntityScalarDataPropertyExistentialRestrictionAxiom])]
  = {
    queue.foldLeft[Try[(OMFSchemaResolver, Map[UUID, Seq[tables.EntityScalarDataPropertyExistentialRestrictionAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)
        val entities = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.entities).toMap
        val scalarDPs = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.entityScalarDataProperties).toMap
        val dataRanges = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.dataranges).toMap
        val (available, remaining) = x.partition { ax =>
          entities.contains(UUID.fromString(ax.restrictedEntityUUID)) &&
            scalarDPs.contains(UUID.fromString(ax.scalarPropertyUUID)) &&
            dataRanges.contains(UUID.fromString(ax.scalarRestrictionUUID))
        }
        val si = available
          .map { ax =>
            impl.EntityScalarDataPropertyExistentialRestrictionAxiom(
              UUID.fromString(ax.uuid),
              entities(UUID.fromString(ax.restrictedEntityUUID)),
              scalarDPs(UUID.fromString(ax.scalarPropertyUUID)),
              dataRanges(UUID.fromString(ax.scalarRestrictionUUID)))
          }
          .to[Set]
        impl.TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = impl.TerminologyContext(ri.context.annotationProperties, gj)),
              mi + (guuid -> remaining),
              fi || si.nonEmpty)
          }
      case (Failure(t), _) =>
        Failure(t)
    } match {
      case Success(Tuple3(r, m, f)) =>
        if (f)
          resolveEntityScalarDataPropertyExistentialRestrictionAxioms(r, m)
        else
          Success(Tuple2(r, m.flatMap(_._2).to[Seq]))
      case Failure(t) =>
        Failure(t)
    }
  }

  def mapEntityScalarDataPropertyParticularRestrictionAxioms
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val g = resolver.context.g
    val (resolvable, unresolved) =
      resolver.queue.entityScalarDataPropertyParticularRestrictionAxioms
        .groupBy(_.graphUUID)
        .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    val r1 = resolver.copy(queue = resolver.queue.copy(entityScalarDataPropertyParticularRestrictionAxioms = unresolved.flatMap(_._2).to[Seq]))
    resolveEntityScalarDataPropertyParticularRestrictionAxioms(r1, resolvable).map {
      case (r2, remaining) =>
        r2.copy(queue = r2.queue.copy(entityScalarDataPropertyParticularRestrictionAxioms = r2.queue.entityScalarDataPropertyParticularRestrictionAxioms ++ remaining))
    }
  }

  final def resolveEntityScalarDataPropertyParticularRestrictionAxioms
  ( resolver: OMFSchemaResolver, queue: Map[UUID, Seq[tables.EntityScalarDataPropertyParticularRestrictionAxiom]] )
  : Try[(OMFSchemaResolver, Seq[tables.EntityScalarDataPropertyParticularRestrictionAxiom])]
  = {
    queue.foldLeft[Try[(OMFSchemaResolver, Map[UUID, Seq[tables.EntityScalarDataPropertyParticularRestrictionAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)
        val entities = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.entities).toMap
        val scalarDPs = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.entityScalarDataProperties).toMap
        val (available, remaining) = x.partition { ax =>
          entities.contains(UUID.fromString(ax.restrictedEntityUUID)) &&
            scalarDPs.contains(UUID.fromString(ax.scalarPropertyUUID))
        }
        val si = available
          .map { ax =>
            impl.EntityScalarDataPropertyParticularRestrictionAxiom(
              UUID.fromString(ax.uuid),
              ax.literalValue,
              entities(UUID.fromString(ax.restrictedEntityUUID)),
              scalarDPs(UUID.fromString(ax.scalarPropertyUUID)))
          }
          .to[Set]
        impl.TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = impl.TerminologyContext(ri.context.annotationProperties, gj)),
              mi + (guuid -> remaining),
              fi || si.nonEmpty)
          }
      case (Failure(t), _) =>
        Failure(t)
    } match {
      case Success(Tuple3(r, m, f)) =>
        if (f)
          resolveEntityScalarDataPropertyParticularRestrictionAxioms(r, m)
        else
          Success(Tuple2(r, m.flatMap(_._2).to[Seq]))
      case Failure(t) =>
        Failure(t)
    }
  }

  def mapEntityScalarDataPropertyUniversalRestrictionAxioms
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val g = resolver.context.g
    val (resolvable, unresolved) =
      resolver.queue.entityScalarDataPropertyUniversalRestrictionAxioms
        .groupBy(_.graphUUID)
        .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    val r1 = resolver.copy(queue = resolver.queue.copy(entityScalarDataPropertyUniversalRestrictionAxioms = unresolved.flatMap(_._2).to[Seq]))
    resolveEntityScalarDataPropertyUniversalRestrictionAxioms(r1, resolvable).map {
      case (r2, remaining) =>
        r2.copy(queue = r2.queue.copy(entityScalarDataPropertyUniversalRestrictionAxioms = r2.queue.entityScalarDataPropertyUniversalRestrictionAxioms ++ remaining))
    }
  }

  final def resolveEntityScalarDataPropertyUniversalRestrictionAxioms
  ( resolver: OMFSchemaResolver, queue: Map[UUID, Seq[tables.EntityScalarDataPropertyUniversalRestrictionAxiom]] )
  : Try[(OMFSchemaResolver, Seq[tables.EntityScalarDataPropertyUniversalRestrictionAxiom])]
  = {
    queue.foldLeft[Try[(OMFSchemaResolver, Map[UUID, Seq[tables.EntityScalarDataPropertyUniversalRestrictionAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)
        val entities = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.entities).toMap
        val scalarDPs = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.entityScalarDataProperties).toMap
        val dataRanges = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.dataranges).toMap
        val (available, remaining) = x.partition { ax =>
          entities.contains(UUID.fromString(ax.restrictedEntityUUID)) &&
            scalarDPs.contains(UUID.fromString(ax.scalarPropertyUUID)) &&
            dataRanges.contains(UUID.fromString(ax.scalarRestrictionUUID))
        }
        val si = available
          .map { ax =>
            impl.EntityScalarDataPropertyUniversalRestrictionAxiom(
              UUID.fromString(ax.uuid),
              entities(UUID.fromString(ax.restrictedEntityUUID)),
              scalarDPs(UUID.fromString(ax.scalarPropertyUUID)),
              dataRanges(UUID.fromString(ax.scalarRestrictionUUID)))
          }
          .to[Set]
        impl.TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = impl.TerminologyContext(ri.context.annotationProperties, gj)),
              mi + (guuid -> remaining),
              fi || si.nonEmpty)
          }
      case (Failure(t), _) =>
        Failure(t)
    } match {
      case Success(Tuple3(r, m, f)) =>
        if (f)
          resolveEntityScalarDataPropertyUniversalRestrictionAxioms(r, m)
        else
          Success(Tuple2(r, m.flatMap(_._2).to[Seq]))
      case Failure(t) =>
        Failure(t)
    }
  }

  def mapAspectSpecializationAxioms
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val g = resolver.context.g
    val (resolvable, unresolved) =
      resolver.queue.aspectSpecializationAxioms
        .groupBy(_.graphUUID)
        .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    val r1 = resolver.copy(queue = resolver.queue.copy(aspectSpecializationAxioms = unresolved.flatMap(_._2).to[Seq]))
    resolveAspectSpecializationAxioms(r1, resolvable).map {
      case (r2, remaining) =>
        r2.copy(queue = r2.queue.copy(aspectSpecializationAxioms = r2.queue.aspectSpecializationAxioms ++ remaining))
    }
  }

  final def resolveAspectSpecializationAxioms
  ( resolver: OMFSchemaResolver, queue: Map[UUID, Seq[tables.AspectSpecializationAxiom]] )
  : Try[(OMFSchemaResolver, Seq[tables.AspectSpecializationAxiom])]
  = {
    queue.foldLeft[Try[(OMFSchemaResolver, Map[UUID, Seq[tables.AspectSpecializationAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)
        val entities = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.entities).toMap
        val aspects = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.aspects).toMap
        val (available, remaining) = x.partition { ax =>
          entities.contains(UUID.fromString(ax.subEntityUUID)) &&
            aspects.contains(UUID.fromString(ax.superAspectUUID))
        }
        val si = available
          .map { ax =>
            impl.AspectSpecializationAxiom(
              UUID.fromString(ax.uuid),
              entities(UUID.fromString(ax.subEntityUUID)),
              aspects(UUID.fromString(ax.superAspectUUID)))
          }
          .to[Set]
        impl.TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = impl.TerminologyContext(ri.context.annotationProperties, gj)),
              mi + (guuid -> remaining),
              fi || si.nonEmpty)
          }
      case (Failure(t), _) =>
        Failure(t)
    } match {
      case Success(Tuple3(r, m, f)) =>
        if (f)
          resolveAspectSpecializationAxioms(r, m)
        else
          Success(Tuple2(r, m.flatMap(_._2).to[Seq]))
      case Failure(t) =>
        Failure(t)
    }
  }

  def mapConceptSpecializationAxioms
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val g = resolver.context.g
    val (resolvable, unresolved) =
      resolver.queue.conceptSpecializationAxioms
        .groupBy(_.graphUUID)
        .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    val r1 = resolver.copy(queue = resolver.queue.copy(conceptSpecializationAxioms = unresolved.flatMap(_._2).to[Seq]))
    resolveConceptSpecializationAxioms(r1, resolvable).map {
      case (r2, remaining) =>
        r2.copy(queue = r2.queue.copy(conceptSpecializationAxioms = r2.queue.conceptSpecializationAxioms ++ remaining))
    }
  }

  final def resolveConceptSpecializationAxioms
  ( resolver: OMFSchemaResolver, queue: Map[UUID, Seq[tables.ConceptSpecializationAxiom]] )
  : Try[(OMFSchemaResolver, Seq[tables.ConceptSpecializationAxiom])]
  = {
    queue.foldLeft[Try[(OMFSchemaResolver, Map[UUID, Seq[tables.ConceptSpecializationAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)
        val concepts = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.concepts).toMap
        val (available, remaining) = x.partition { ax =>
          concepts.contains(UUID.fromString(ax.subConceptUUID)) &&
            concepts.contains(UUID.fromString(ax.superConceptUUID))
        }
        val si = available
          .map { ax =>
            impl.ConceptSpecializationAxiom(
              UUID.fromString(ax.uuid),
              concepts(UUID.fromString(ax.subConceptUUID)),
              concepts(UUID.fromString(ax.superConceptUUID)))
          }
          .to[Set]
        impl.TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = impl.TerminologyContext(ri.context.annotationProperties, gj)),
              mi + (guuid -> remaining),
              fi || si.nonEmpty)
          }
      case (Failure(t), _) =>
        Failure(t)
    } match {
      case Success(Tuple3(r, m, f)) =>
        if (f)
          resolveConceptSpecializationAxioms(r, m)
        else
          Success(Tuple2(r, m.flatMap(_._2).to[Seq]))
      case Failure(t) =>
        Failure(t)
    }
  }

  def mapReifiedRelationshipSpecializationAxioms
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val g = resolver.context.g
    val (resolvable, unresolved) =
      resolver.queue.reifiedRelationshipSpecializationAxioms
        .groupBy(_.graphUUID)
        .map { case (uuid, ranges) => UUID.fromString(uuid) -> ranges }
        .partition { case (graphUUID, _) => ns.contains(graphUUID) }

    val r1 = resolver.copy(queue = resolver.queue.copy(reifiedRelationshipSpecializationAxioms = unresolved.flatMap(_._2).to[Seq]))
    resolveReifiedRelationshipSpecializationAxioms(r1, resolvable).map {
      case (r2, remaining) =>
        r2.copy(queue = r2.queue.copy(reifiedRelationshipSpecializationAxioms = r2.queue.reifiedRelationshipSpecializationAxioms ++ remaining))
    }
  }

  final def resolveReifiedRelationshipSpecializationAxioms
  ( resolver: OMFSchemaResolver, queue: Map[UUID, Seq[tables.ReifiedRelationshipSpecializationAxiom]] )
  : Try[(OMFSchemaResolver, Seq[tables.ReifiedRelationshipSpecializationAxiom])]
  = {
    queue.foldLeft[Try[(OMFSchemaResolver, Map[UUID, Seq[tables.ReifiedRelationshipSpecializationAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)
        val reifiedRelationships = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.reifiedRelationships).toMap
        val (available, remaining) = x.partition { ax =>
          reifiedRelationships.contains(UUID.fromString(ax.subRelationshipUUID)) &&
            reifiedRelationships.contains(UUID.fromString(ax.superRelationshipUUID))
        }
        val si = available
          .map { ax =>
            impl.ReifiedRelationshipSpecializationAxiom(
              UUID.fromString(ax.uuid),
              reifiedRelationships(UUID.fromString(ax.subRelationshipUUID)),
              reifiedRelationships(UUID.fromString(ax.superRelationshipUUID)))
          }
          .to[Set]
        impl.TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = impl.TerminologyContext(ri.context.annotationProperties, gj)),
              mi + (guuid -> remaining),
              fi || si.nonEmpty)
          }
      case (Failure(t), _) =>
        Failure(t)
    } match {
      case Success(Tuple3(r, m, f)) =>
        if (f)
          resolveReifiedRelationshipSpecializationAxioms(r, m)
        else
          Success(Tuple2(r, m.flatMap(_._2).to[Seq]))
      case Failure(t) =>
        Failure(t)
    }
  }

  def mapRootConceptTaxonomyAxioms
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.bundles
    val g = resolver.context.g
    val (resolvable, unresolved) =
      resolver.queue.rootConceptTaxonomyAxioms
        .groupBy(_.bundleUUID)
        .map { case (uuid, axioms) => UUID.fromString(uuid) -> axioms }
        .partition { case (bundleUUID, _) => ns.contains(bundleUUID) }

    val r1 = resolver.copy(queue = resolver.queue.copy(rootConceptTaxonomyAxioms = unresolved.flatMap(_._2).to[Seq]))
    resolveRootConceptTaxonomyAxioms(r1, resolvable).map {
      case (r2, remaining) =>
        r2.copy(queue = r2.queue.copy(rootConceptTaxonomyAxioms = r2.queue.rootConceptTaxonomyAxioms ++ remaining))
    }
  }

  final def resolveRootConceptTaxonomyAxioms
  ( resolver: OMFSchemaResolver, queue: Map[UUID, Seq[tables.RootConceptTaxonomyAxiom]] )
  : Try[(OMFSchemaResolver, Seq[tables.RootConceptTaxonomyAxiom])]
  = {
    queue.foldLeft[Try[(OMFSchemaResolver, Map[UUID, Seq[tables.RootConceptTaxonomyAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (buuid, x)) =>
        val gi = ri.context.g
        val bundle = ri.context.bundles(buuid)
        val concepts = gi.outerNodeTraverser(gi.get(bundle)).flatMap(_.concepts).toMap
        val (available, remaining) = x.partition { ax =>
          concepts.contains(UUID.fromString(ax.rootUUID))
        }
        val si = available
          .map { ax =>
            impl.RootConceptTaxonomyAxiom(
              UUID.fromString(ax.uuid),
              concepts(UUID.fromString(ax.rootUUID)))
          }
          .to[Set]
        impl.TerminologyContext
          .replaceNode(gi, bundle, bundle.withBundleStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = impl.TerminologyContext(ri.context.annotationProperties, gj)),
              mi + (buuid -> remaining),
              fi || si.nonEmpty)
          }
      case (Failure(t), _) =>
        Failure(t)
    } match {
      case Success(Tuple3(r, m, f)) =>
        if (f)
          resolveRootConceptTaxonomyAxioms(r, m)
        else
          Success(Tuple2(r, m.flatMap(_._2).to[Seq]))
      case Failure(t) =>
        Failure(t)
    }
  }

  def mapAnonymousConceptTaxonomyAxioms
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.bundles
    val g = resolver.context.g
    val (resolvable, unresolved) =
      resolver.queue.anonymousConceptTaxonomyAxioms
        .groupBy(_.bundleUUID)
        .map { case (uuid, axioms) => UUID.fromString(uuid) -> axioms }
        .partition { case (bundleUUID, _) => ns.contains(bundleUUID) }

    val r1 = resolver.copy(queue = resolver.queue.copy(anonymousConceptTaxonomyAxioms = unresolved.flatMap(_._2).to[Seq]))
    resolveAnonymousConceptTaxonomyAxioms(r1, resolvable).map {
      case (r2, remaining) =>
        r2.copy(queue = r2.queue.copy(anonymousConceptTaxonomyAxioms = r2.queue.anonymousConceptTaxonomyAxioms ++ remaining))
    }
  }

  final def resolveAnonymousConceptTaxonomyAxioms
  (resolver: OMFSchemaResolver, queue: Map[UUID, Seq[tables.AnonymousConceptTaxonomyAxiom]])
  : Try[(OMFSchemaResolver, Seq[tables.AnonymousConceptTaxonomyAxiom])]
  = {
    queue
      .foldLeft[Try[(OMFSchemaResolver, Map[UUID, Seq[tables.AnonymousConceptTaxonomyAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (buuid, x)) =>
        val gi = ri.context.g
        val bundle = ri.context.bundles(buuid)
        val accessible = bundle.bundleStatements
          .flatMap {
            case ctd: api.ConceptTreeDisjunction =>
              Some(ctd.uuid -> ctd)
            case _ =>
              None
          }
          .toMap
        val (available, remaining) = x.partition { ax =>
          accessible.contains(UUID.fromString(ax.disjointTaxonomyParentUUID))
        }
        val si = available
          .map { ax =>
            impl.AnonymousConceptTaxonomyAxiom(
              UUID.fromString(ax.uuid),
              accessible(UUID.fromString(ax.disjointTaxonomyParentUUID)))
          }
          .to[Set]
        impl.TerminologyContext
          .replaceNode(gi, bundle, bundle.withBundleStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = impl.TerminologyContext(ri.context.annotationProperties, gj)),
              mi + (buuid -> remaining),
              fi || si.nonEmpty)
          }
      case (Failure(t), _) =>
        Failure(t)
    } match {
      case Success(Tuple3(r, m, f)) =>
        if (f)
          resolveAnonymousConceptTaxonomyAxioms(r, m)
        else
          Success(Tuple2(r, m.flatMap(_._2).to[Seq]))
      case Failure(t) =>
        Failure(t)
    }
  }

  def mapSpecificDisjointConceptAxioms
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.bundles
    val g = resolver.context.g
    val (resolvable, unresolved) =
      resolver.queue.specificDisjointConceptAxioms
        .groupBy(_.bundleUUID)
        .map { case (uuid, axioms) => UUID.fromString(uuid) -> axioms }
        .partition { case (bundleUUID, _) => ns.contains(bundleUUID) }

    val r1 = resolver.copy(queue = resolver.queue.copy(specificDisjointConceptAxioms = unresolved.flatMap(_._2).to[Seq]))
    resolveSpecificDisjointConceptAxioms(r1, resolvable).map {
      case (r2, remaining) =>
        r2.copy(queue = r2.queue.copy(specificDisjointConceptAxioms = r2.queue.specificDisjointConceptAxioms ++ remaining))
    }
  }

  final def resolveSpecificDisjointConceptAxioms
  (resolver: OMFSchemaResolver, queue: Map[UUID, Seq[tables.SpecificDisjointConceptAxiom]])
  : Try[(OMFSchemaResolver, Seq[tables.SpecificDisjointConceptAxiom])]
  = {
    queue
      .foldLeft[Try[(OMFSchemaResolver, Map[UUID, Seq[tables.SpecificDisjointConceptAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (buuid, x)) =>
        val gi = ri.context.g
        val bundle = ri.context.bundles(buuid)
        val concepts = gi.outerNodeTraverser(gi.get(bundle)).flatMap(_.concepts).toMap
        val accessible = bundle.bundleStatements
          .flatMap {
            case sdc: api.ConceptTreeDisjunction =>
              Some(sdc.uuid -> sdc)
            case _ =>
              None
          }
          .toMap
        val (available, remaining) = x.partition { ax =>
          accessible.contains(UUID.fromString(ax.disjointTaxonomyParentUUID)) &&
            concepts.contains(UUID.fromString(ax.disjointLeafUUID))
        }
        val si = available
          .map { ax =>
            impl.SpecificDisjointConceptAxiom(
              UUID.fromString(ax.uuid),
              concepts(UUID.fromString(ax.disjointLeafUUID)),
              accessible(UUID.fromString(ax.disjointTaxonomyParentUUID)))
          }
          .to[Set]
        impl.TerminologyContext
          .replaceNode(gi, bundle, bundle.withBundleStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = impl.TerminologyContext(ri.context.annotationProperties, gj)),
              mi + (buuid -> remaining),
              fi || si.nonEmpty)
          }
      case (Failure(t), _) =>
        Failure(t)
    } match {
      case Success(Tuple3(r, m, f)) =>
        if (f)
          resolveSpecificDisjointConceptAxioms(r, m)
        else
          Success(Tuple2(r, m.flatMap(_._2).to[Seq]))
      case Failure(t) =>
        Failure(t)
    }
  }

  type AnnotationMap = Map[tables.AnnotationProperty, Seq[tables.Annotation]]

  def annotationMapS
  (t2everything: Map[tables.UUID, Set[tables.UUID]])
  (q_u: (AnnotationMap, AnnotationMap),
   ap_as: (tables.AnnotationProperty, Seq[tables.Annotation]))
  : (AnnotationMap, AnnotationMap)
  = {
    val (q, u) = q_u
    val (ap, as) = ap_as

    val (t_resolvable, t_unresolved) =
      as.partition(a => t2everything.contains(a.terminologyUUID))

    val (s_resolvable, s_unresolved) =
      t_resolvable.partition { a =>
        t2everything
          .get(a.terminologyUUID)
          .exists(_.contains(a.subjectUUID))
      }

    q_u
  }

  def annotationMapC
  (q_u1: (AnnotationMap, AnnotationMap),
   q_u2: (AnnotationMap, AnnotationMap))
  : (AnnotationMap, AnnotationMap)
  = {
    val (q1, u1) = q_u1
    val (q2, u2) = q_u2
    require(q1.keySet.intersect(q2.keySet).isEmpty)
    require(u1.keySet.intersect(u2.keySet).isEmpty)
    Tuple2( q1 ++ q2, u1 ++ u2)
  }

  def mapAnnotationPairs
  (resolver: OMFSchemaResolver)
  : Try[OMFSchemaResolver]
  = {
    val ns = resolver.context.nodes
    val t2everything = ns.map { case (uuid, tbox) =>
        uuid.toString -> tbox.everything().map(_.uuid.toString)
    }
    val g = resolver.context.g

    val (resolved, remaining)
    = resolver.queue.annotations
      .aggregate[(AnnotationMap, AnnotationMap)](Map.empty, Map.empty)(
      seqop = annotationMapS(t2everything),
      combop = annotationMapC)

    val r = resolver.copy(queue = resolver.queue.copy(annotations = remaining))
    Success(r)
  }

  /*

  final def resolveAnnotationPairs
  (resolver: OMFSchemaResolver,
   queue: Map[tables.AnnotationProperty, Seq[tables.Annotation]],
   remaining: Map[tables.AnnotationProperty, Seq[tables.Annotation]])
  : Try[(OMFSchemaResolver, Map[tables.AnnotationProperty, Seq[tables.Annotation]])]
  = {
    queue
      .foldLeft[Try[(OMFSchemaResolver, Map[UUID, Seq[tables.Annotation]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (uuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(uuid)
        val statements = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[UUID, _ <: api.TerminologyBoxStatement]](Map.empty) { case (acc, box) =>
          acc ++ box.boxStatements.map(s => s.uuid -> s).toMap
        }

        val (available, remaining) = x.partition { a =>
          statements.contains(UUID.fromString(a.subjectUUID)) &&
          resolver.context.annotationProperties.contains(UUID.fromString(a.propertyUUID))
        }
        val si = available
          .map { a =>
            impl.Annotation(
              statements(UUID.fromString(a.subjectUUID)),
              resolver.context.annotationProperties(UUID.fromString(a.propertyUUID)),
              a.value)
          }
          .to[Set]
        impl.TerminologyContext
          .replaceNode(gi, tbox, tbox.withAnnotations(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = impl.TerminologyContext(ri.context.annotationProperties, gj)),
              mi + (uuid -> remaining),
              fi || si.nonEmpty)
          }
      case (Failure(t), _) =>
        Failure(t)
    } match {
      case Success(Tuple3(r, m, f)) =>
        if (f)
          resolveAnnotationPairs(r, m)
        else
          Success(Tuple2(r, m.flatMap(_._2).to[Seq]))
      case Failure(t) =>
        Failure(t)
    }
  }
  */
}
