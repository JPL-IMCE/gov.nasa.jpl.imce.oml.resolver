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

package gov.nasa.jpl.imce.oml.specification.resolver

import java.util.UUID

import gov.nasa.jpl.imce.oml.specification._

import scalax.collection.immutable.Graph
import scala.collection.immutable.{Map, Seq, SortedSet, TreeSet}
import scala.collection.parallel.immutable.ParSeq
import scala.{Boolean, None, Ordering, Some, StringContext, Tuple2, Tuple3}
import scala.util.{Failure, Success, Try}
import scala.Predef.ArrowAssoc

case class OMLTablesResolver private[resolver]
(context: TerminologyContext,
 queue: tables.OMLSpecificationTables)

object OMLTablesResolver {

  def resolve(t: tables.OMLSpecificationTables)
  : Try[OMLTablesResolver]
  = for {
    init <- Try.apply(OMLTablesResolver(
      resolver.TerminologyContext(t.annotationProperties.map { ap =>
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
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
  = {
    val gN = resolver.queue.terminologyGraphs.foldLeft(resolver.context.g) { (gi, t) =>
      gi + impl.TerminologyGraph(
        java.util.UUID.fromString(t.uuid), t.kind, t.name, t.iri,
        annotations=TreeSet.empty[api.Annotation],
        boxStatements=TreeSet.empty[api.TerminologyBoxStatement])
    }

    val r = resolver.copy(
      context = resolver.context.copy(g = gN),
      queue = resolver.queue.copy(terminologyGraphs = Seq()))
    Success(r)
  }

  def mapBundles
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
  = {
    val gN = resolver.queue.bundles.foldLeft(resolver.context.g) { (gi, b) =>
      gi + impl.Bundle(java.util.UUID.fromString(b.uuid), b.kind, b.name, b.iri,
        annotations=TreeSet.empty[api.Annotation],
        boxStatements=TreeSet.empty[api.TerminologyBoxStatement],
        bundleStatements=TreeSet.empty[api.TerminologyBundleStatement],
        terminologyBundleAxioms=TreeSet.empty[api.TerminologyBundleAxiom])
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
  (g1: Graph[api.TerminologyBox, TerminologyEdge],
   g2: Graph[api.TerminologyBox, TerminologyEdge])
  : Graph[api.TerminologyBox, TerminologyEdge]
  = g1.union(g2)

  type HyperGraphV = Try[Graph[api.TerminologyBox, TerminologyEdge]]

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

      val s = entry
        ._2
        .foldLeft[SortedSet[api.TerminologyBoxStatement]](TreeSet.empty[api.TerminologyBoxStatement]) {
        case (acc, e) =>
          acc + impl.Aspect(graph = n0, uuid = UUID.fromString(e.uuid), name = e.name)
      }

      val result = resolver.TerminologyContext.replaceNode(g, n0, n0.withBoxStatements(s))
      result
    }
  }

  def mapAspects
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
            context = TerminologyContext(resolver.context.annotationProperties, g),
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

      val s =
        entry
          ._2
          .foldLeft[SortedSet[api.TerminologyBoxStatement]](TreeSet.empty[api.TerminologyBoxStatement]) {
          case (acc, e) =>
            acc + impl.Concept(graph = n0, uuid = UUID.fromString(e.uuid), isAbstract = e.isAbstract, name = e.name)
        }

      val result = resolver.TerminologyContext.replaceNode(g, n0, n0.withBoxStatements(s))
      result
    }
  }

  def mapConcepts
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
            context = TerminologyContext(resolver.context.annotationProperties, g),
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

      val s = entry
        ._2
        .foldLeft[SortedSet[api.TerminologyBoxStatement]](TreeSet.empty[api.TerminologyBoxStatement]) {
        case (acc, e) =>
          acc + impl.Scalar(graph = n0, uuid = UUID.fromString(e.uuid), name = e.name)
      }

      val result = resolver.TerminologyContext.replaceNode(g, n0, n0.withBoxStatements(s))
      result
    }
  }

  def mapScalars
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
            context = TerminologyContext(resolver.context.annotationProperties, g),
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
      val s = entry
        ._2
        .foldLeft[SortedSet[api.TerminologyBoxStatement]](TreeSet.empty[api.TerminologyBoxStatement]) {
        case (acc, e) =>
          acc + impl.Structure(graph = n0, uuid = UUID.fromString(e.uuid), name = e.name)
      }

      val result = resolver.TerminologyContext.replaceNode(g, n0, n0.withBoxStatements(s))
      result
    }
  }

  def mapStructures
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
            context = TerminologyContext(resolver.context.annotationProperties, g),
            queue = resolver.queue.copy(structures = unresolvable.flatMap(_._2).to[Seq]))
      }
  }

  def seqopTerminologyExtends
  (nodes: Map[UUID, api.TerminologyBox])
  (g: Graph[api.TerminologyBox, TerminologyEdge],
   entry: ((UUID, UUID), tables.TerminologyExtensionAxiom))
  : Graph[api.TerminologyBox, TerminologyEdge]
  = {
    val extending = nodes(entry._1._1)
    val extended = nodes(entry._1._2)

    val result = g + resolver.TerminologyEdge(
      extending, extended,
      impl.TerminologyExtensionAxiom(
        uuid = UUID.fromString(entry._2.uuid),
        extendingTerminology = extending,
        extendedTerminology = extended))

    result
  }

  def mapTerminologyExtends
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
          context = TerminologyContext(resolver.context.annotationProperties, g),
          queue = resolver.queue.copy(terminologyExtensionAxioms = s))

    Success(r)
  }

  def seqopTerminologyNesting
  (ns: Map[tables.UUID, api.TerminologyGraph],
   cs: Map[tables.UUID, (api.Concept, api.TerminologyBox)])
  (g: Graph[api.TerminologyBox, TerminologyEdge],
   entry: ((tables.UUID, tables.UUID), tables.TerminologyNestingAxiom))
  : Graph[api.TerminologyBox, TerminologyEdge]
  = {
    val nestedT = ns(entry._1._1)
    val (nestingC, nestingT) = cs(entry._1._2)

    val result = g + resolver.TerminologyEdge(
      nestedT, nestingT,
      impl.TerminologyNestingAxiom(
        uuid = UUID.fromString(entry._2.uuid),
        nestedTerminology = nestedT,
        nestingContext = nestingC,
        nestingTerminology = nestingT))
    result
  }

  def mapTerminologyNestings
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
  = {
    val ns
    : Map[tables.UUID, api.TerminologyGraph]
    = resolver.context.nodes.flatMap {
      case (uuid, g: api.TerminologyGraph) =>
        Some(uuid.toString -> g)
      case _ =>
        None
    }

    val referencableConcepts
    : Map[tables.UUID, (api.Concept, api.TerminologyBox)]
    = resolver.context.g.nodes.toOuter
      .foldLeft[Map[tables.UUID, (api.Concept, api.TerminologyBox)]](Map.empty) { case (acc, t) =>
      acc ++
      t.concepts().map { c =>
        c.uuid.toString -> (c -> t)
      }
    }

    val byUUID
    : Seq[((tables.UUID, tables.UUID), tables.TerminologyNestingAxiom)]
    = resolver.queue.terminologyNestingAxioms
      .map { tAxiom =>
        (tAxiom.nestedTerminologyUUID -> tAxiom.nestingContextUUID) -> tAxiom
      }

    val (resolvable, unresolvable) =
      byUUID
        .partition { case (graphUUIDPair, tx) =>
          ns.contains(graphUUIDPair._1) &&
            referencableConcepts.get(graphUUIDPair._2).fold[Boolean](false){ case (c,tbox) =>
                tbox.uuid == UUID.fromString(tx.nestingTerminologyUUID)
            }
        }

    val g =
      resolvable
        .aggregate(resolver.context.g)(seqop = seqopTerminologyNesting(ns, referencableConcepts), combop = combopGraphs)

    val s =
      unresolvable.map(_._2).seq

    val r =
      resolver
        .copy(
          context = TerminologyContext(resolver.context.annotationProperties, g),
          queue = resolver.queue.copy(terminologyNestingAxioms = s))

    Success(r)
  }

  def seqopConceptDesignationTerminology
  (ns: Map[tables.UUID, api.TerminologyGraph],
   cs: Map[tables.UUID, (api.Concept, api.TerminologyBox)])
  (g: Graph[api.TerminologyBox, TerminologyEdge],
   entry: ((tables.UUID, tables.UUID), tables.ConceptDesignationTerminologyAxiom))
  : Graph[api.TerminologyBox, TerminologyEdge]
  = {
    val designationG = ns(entry._1._1)
    val (designatedC, designatedTBox) = cs(entry._1._2)

    val result = g + resolver.TerminologyEdge(
      designationG, designatedTBox,
      impl.ConceptDesignationTerminologyAxiom(
        uuid = UUID.fromString(entry._2.uuid),
        designatedConcept = designatedC,
        designatedTerminology = designatedTBox,
        designationTerminologyGraph = designationG))

    result
  }

  def mapConceptDesignationTerminologyAxioms
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
  = {
    val ns
    : Map[tables.UUID, api.TerminologyGraph]
    = resolver.context.nodes.flatMap {
      case (uuid, g: api.TerminologyGraph) =>
        Some(uuid.toString -> g)
      case _ =>
        None
    }

    val referencableConcepts
    : Map[tables.UUID, (api.Concept, api.TerminologyBox)]
    = resolver.context.g.nodes.toOuter
      .foldLeft[Map[tables.UUID, (api.Concept, api.TerminologyBox)]](Map.empty) { case (acc, t) =>
      acc ++
        t.concepts().map { c =>
          c.uuid.toString -> (c -> t)
        }
    }

    val byUUID
    : Seq[((tables.UUID, tables.UUID), tables.ConceptDesignationTerminologyAxiom)]
    = resolver.queue.conceptDesignationTerminologyAxioms
      .map { tAxiom =>
        (tAxiom.designationTerminologyGraphUUID -> tAxiom.designatedConceptUUID) -> tAxiom
      }

    val (resolvable, unresolvable) =
      byUUID
        .partition { case (graphUUIDPair, _) =>
          ns.contains(graphUUIDPair._1) &&
            referencableConcepts.contains(graphUUIDPair._2)
        }

    val g =
      resolvable
        .aggregate(resolver.context.g)(seqop = seqopConceptDesignationTerminology(ns, referencableConcepts), combop = combopGraphs)

    val s =
      unresolvable.map(_._2).seq

    val r =
      resolver
        .copy(
          context = TerminologyContext(resolver.context.annotationProperties, g),
          queue = resolver.queue.copy(conceptDesignationTerminologyAxioms = s))

    Success(r)
  }

  def seqopBundledTerminologyAxioms
  (bundles: Map[UUID, api.Bundle],
   nodes: Map[UUID, api.TerminologyBox])
  (g: Graph[api.TerminologyBox, TerminologyEdge],
   entry: ((UUID, UUID), tables.BundledTerminologyAxiom))
  : Graph[api.TerminologyBox, TerminologyEdge]
  = {
    val bundling = bundles(entry._1._1)
    val bundled = nodes(entry._1._2)

    val result = g + resolver.TerminologyEdge(
      bundling, bundled,
      impl.BundledTerminologyAxiom(
        uuid = UUID.fromString(entry._2.uuid),
        terminologyBundle = bundling,
        bundledTerminology = bundled))

    result
  }

  def mapBundledTerminologyAxioms
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
          context = TerminologyContext(resolver.context.annotationProperties, g),
          queue = resolver.queue.copy(bundledTerminologyAxioms = s))

    Success(r)
  }

  def mapRestrictedDataRanges
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
  (resolver: OMLTablesResolver, queue: Map[UUID, Seq[tables.ReifiedRelationship]])
  : Try[(OMLTablesResolver, Seq[tables.ReifiedRelationship])]
  = {
    queue
      .foldLeft[Try[(OMLTablesResolver, Map[UUID, Seq[tables.ReifiedRelationship]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)

        val referencableEntities
        : Map[tables.UUID, api.Entity]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.Entity]](Map.empty)(_ ++ _.entities().map(e => e.uuid.toString -> e))

        val (available, remaining) = x.partition { rr =>
          referencableEntities.contains(rr.sourceUUID) && referencableEntities.contains(rr.targetUUID)
        }

        val si
        : SortedSet[api.TerminologyBoxStatement]
        = available
          .foldLeft[SortedSet[api.TerminologyBoxStatement]](TreeSet.empty[api.TerminologyBoxStatement]) {
          case (acc, rr) =>
            acc +
              impl.ReifiedRelationship(
                tbox,
                UUID.fromString(rr.uuid),
                rr.isAbstract,
                rr.name,
                rr.unreifiedPropertyName,
                rr.unreifiedInversePropertyName,
                rr.isAsymmetric,
                rr.isEssential,
                rr.isFunctional,
                rr.isInverseEssential,
                rr.isInverseFunctional,
                rr.isIrreflexive,
                rr.isReflexive,
                rr.isSymmetric,
                rr.isTransitive,
                referencableEntities(rr.sourceUUID),
                referencableEntities(rr.targetUUID))
        }

        TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = TerminologyContext(ri.context.annotationProperties, gj)),
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
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
  (resolver: OMLTablesResolver, queue: Map[UUID, Seq[tables.UnreifiedRelationship]] )
  : Try[(OMLTablesResolver, Seq[tables.UnreifiedRelationship])]
  = {
    queue
      .foldLeft[Try[(OMLTablesResolver, Map[UUID, Seq[tables.UnreifiedRelationship]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)

        val referencableEntities
        : Map[tables.UUID, api.Entity]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.Entity]](Map.empty)(_ ++ _.entities().map(e => e.uuid.toString -> e))

        val (available, remaining) = x.partition { ur =>
          referencableEntities.contains(ur.sourceUUID) && referencableEntities.contains(ur.targetUUID)
        }

        val si
        : SortedSet[api.TerminologyBoxStatement]
        = available
          .foldLeft[SortedSet[api.TerminologyBoxStatement]](TreeSet.empty[api.TerminologyBoxStatement]) {
          case (acc, ur) =>
            acc +
              impl.UnreifiedRelationship(
                tbox,
                UUID.fromString(ur.uuid),
                ur.name,
                ur.isAsymmetric,
                ur.isEssential,
                ur.isFunctional,
                ur.isInverseEssential,
                ur.isInverseFunctional,
                ur.isIrreflexive,
                ur.isReflexive,
                ur.isSymmetric,
                ur.isTransitive,
                referencableEntities(ur.sourceUUID),
                referencableEntities(ur.targetUUID))
        }

          TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = TerminologyContext(ri.context.annotationProperties, gj)),
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
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
  (resolver: OMLTablesResolver, queue: Map[UUID, Seq[tables.EntityScalarDataProperty]] )
  : Try[(OMLTablesResolver, Seq[tables.EntityScalarDataProperty])]
  = {
    queue
      .foldLeft[Try[(OMLTablesResolver, Map[UUID, Seq[tables.EntityScalarDataProperty]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)

        val referencableEntities
        : Map[tables.UUID, api.Entity]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.Entity]](Map.empty)(_ ++ _.entities().map(e => e.uuid.toString -> e))

        val referencableDataRanges
        : Map[tables.UUID, api.DataRange]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.DataRange]](Map.empty)(_ ++ _.dataranges().map(dr => dr.uuid.toString -> dr))

        val (available, remaining) = x.partition { dr =>
          referencableEntities.contains(dr.domainUUID) &&
            referencableDataRanges.contains(dr.rangeUUID)
        }

        val si
        : SortedSet[api.TerminologyBoxStatement]
        = available
          .foldLeft[SortedSet[api.TerminologyBoxStatement]](TreeSet.empty[api.TerminologyBoxStatement]) {
          case (acc, dr) =>
            acc +
              impl.EntityScalarDataProperty(
                tbox,
                UUID.fromString(dr.uuid),
                dr.name,
                referencableEntities(dr.domainUUID),
                referencableDataRanges(dr.rangeUUID))
        }

        TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = TerminologyContext(ri.context.annotationProperties, gj)),
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
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
  (resolver: OMLTablesResolver, queue: Map[UUID, Seq[tables.EntityStructuredDataProperty]] )
  : Try[(OMLTablesResolver, Seq[tables.EntityStructuredDataProperty])]
  = {
    queue
      .foldLeft[Try[(OMLTablesResolver, Map[UUID, Seq[tables.EntityStructuredDataProperty]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)

        val referencableEntities
        : Map[tables.UUID, api.Entity]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.Entity]](Map.empty)(_ ++ _.entities().map(e => e.uuid.toString -> e))

        val referencableStructures
        : Map[tables.UUID, api.Structure]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.Structure]](Map.empty)(_ ++ _.structures().map(dr => dr.uuid.toString -> dr))

        val (available, remaining) = x.partition { dr =>
          referencableEntities.contains(dr.domainUUID) &&
            referencableStructures.contains(dr.rangeUUID)
        }

        val si
        : SortedSet[api.TerminologyBoxStatement]
        = available
          .foldLeft[SortedSet[api.TerminologyBoxStatement]](TreeSet.empty[api.TerminologyBoxStatement]) {
          case (acc, dr) =>
            acc +
            impl.EntityStructuredDataProperty(
              tbox,
              UUID.fromString(dr.uuid),
              dr.name,
              referencableEntities(dr.domainUUID),
              referencableStructures(dr.rangeUUID))
        }

        TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = TerminologyContext(ri.context.annotationProperties, gj)),
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
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
  (resolver: OMLTablesResolver, queue: Map[UUID, Seq[tables.ScalarDataProperty]] )
  : Try[(OMLTablesResolver, Seq[tables.ScalarDataProperty])]
  = {
    queue
      .foldLeft[Try[(OMLTablesResolver, Map[UUID, Seq[tables.ScalarDataProperty]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)

        val referencableStructures
        : Map[tables.UUID, api.Structure]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.Structure]](Map.empty)(_ ++ _.structures().map(dr => dr.uuid.toString -> dr))

        val referencableDataRanges
        : Map[tables.UUID, api.DataRange]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.DataRange]](Map.empty)(_ ++ _.dataranges().map(dr => dr.uuid.toString -> dr))

        val (available, remaining) = x.partition { dr =>
          referencableStructures.contains(dr.domainUUID) &&
            referencableDataRanges.contains(dr.rangeUUID)
        }

        val si
        : SortedSet[api.TerminologyBoxStatement]
        = available
          .foldLeft[SortedSet[api.TerminologyBoxStatement]](TreeSet.empty[api.TerminologyBoxStatement]) {
          case (acc, dr) =>
            acc +
            impl.ScalarDataProperty(
              tbox,
              UUID.fromString(dr.uuid),
              dr.name,
              referencableStructures(dr.domainUUID),
              referencableDataRanges(dr.rangeUUID))
        }

        TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = TerminologyContext(ri.context.annotationProperties, gj)),
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
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
  (resolver: OMLTablesResolver, queue: Map[UUID, Seq[tables.StructuredDataProperty]] )
  : Try[(OMLTablesResolver, Seq[tables.StructuredDataProperty])]
  = {
    queue
      .foldLeft[Try[(OMLTablesResolver, Map[UUID, Seq[tables.StructuredDataProperty]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)

        val referencableStructures
        : Map[tables.UUID, api.Structure]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.Structure]](Map.empty)(_ ++ _.structures().map(dr => dr.uuid.toString -> dr))

        val (available, remaining) = x.partition { dr =>
          referencableStructures.contains(dr.domainUUID) &&
            referencableStructures.contains(dr.rangeUUID)
        }

        val si
        : SortedSet[api.TerminologyBoxStatement]
        = available
          .foldLeft[SortedSet[api.TerminologyBoxStatement]](TreeSet.empty[api.TerminologyBoxStatement]) {
          case (acc, dr) =>
            acc +
            impl.StructuredDataProperty(
              tbox,
              UUID.fromString(dr.uuid),
              dr.name,
              referencableStructures(dr.domainUUID),
              referencableStructures(dr.rangeUUID))
        }

        TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = TerminologyContext(ri.context.annotationProperties, gj)),
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
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
  (resolver: OMLTablesResolver, queue: Map[UUID, Seq[tables.ScalarOneOfLiteralAxiom]] )
  : Try[(OMLTablesResolver, Seq[tables.ScalarOneOfLiteralAxiom])]
  = {
    queue.foldLeft[Try[(OMLTablesResolver, Map[UUID, Seq[tables.ScalarOneOfLiteralAxiom]], Boolean)]](
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
        val si
        : SortedSet[api.TerminologyBoxStatement]
        = available
          .foldLeft[SortedSet[api.TerminologyBoxStatement]](TreeSet.empty[api.TerminologyBoxStatement]) {
          case (acc, ax) =>
            acc +
              impl.ScalarOneOfLiteralAxiom(
                tbox,
                UUID.fromString(ax.uuid),
                scalarOneOfRestrictions(UUID.fromString(ax.axiomUUID)),
                ax.value)
        }

        TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = TerminologyContext(ri.context.annotationProperties, gj)),
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
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
  (resolver: OMLTablesResolver, queue: Map[UUID, Seq[tables.EntityExistentialRestrictionAxiom]] )
  : Try[(OMLTablesResolver, Seq[tables.EntityExistentialRestrictionAxiom])]
  = {
    queue.foldLeft[Try[(OMLTablesResolver, Map[UUID, Seq[tables.EntityExistentialRestrictionAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)

        val referencableEntities
        : Map[tables.UUID, api.Entity]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.Entity]](Map.empty)(_ ++ _.entities().map(e => e.uuid.toString -> e))

        val referencableReifiedRelationships
        : Map[tables.UUID, api.ReifiedRelationship]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.ReifiedRelationship]](Map.empty)(_ ++ _.reifiedRelationships().map(e => e.uuid.toString -> e))

        val (available, remaining) = x.partition { ax =>
          referencableEntities.contains(ax.restrictedDomainUUID) &&
            referencableEntities.contains(ax.restrictedRangeUUID) &&
            referencableReifiedRelationships.contains(ax.restrictedRelationUUID)
        }

        val si
        : SortedSet[api.TerminologyBoxStatement]
        = available
          .foldLeft[SortedSet[api.TerminologyBoxStatement]](TreeSet.empty[api.TerminologyBoxStatement]) {
          case (acc, ax) =>
            acc +
            impl.EntityExistentialRestrictionAxiom(
              tbox,
              UUID.fromString(ax.uuid),
              referencableEntities(ax.restrictedDomainUUID),
              referencableEntities(ax.restrictedRangeUUID),
              referencableReifiedRelationships(ax.restrictedRelationUUID))
          }

        TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = TerminologyContext(ri.context.annotationProperties, gj)),
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
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
  (resolver: OMLTablesResolver, queue: Map[UUID, Seq[tables.EntityUniversalRestrictionAxiom]] )
  : Try[(OMLTablesResolver, Seq[tables.EntityUniversalRestrictionAxiom])]
  = {
    queue.foldLeft[Try[(OMLTablesResolver, Map[UUID, Seq[tables.EntityUniversalRestrictionAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)

        val referencableEntities
        : Map[tables.UUID, api.Entity]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.Entity]](Map.empty)(_ ++ _.entities().map(e => e.uuid.toString -> e))

        val referencableReifiedRelationships
        : Map[tables.UUID, api.ReifiedRelationship]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.ReifiedRelationship]](Map.empty)(_ ++ _.reifiedRelationships().map(e => e.uuid.toString -> e))

        val (available, remaining) = x.partition { ax =>
          referencableEntities.contains(ax.restrictedDomainUUID) &&
            referencableEntities.contains(ax.restrictedRangeUUID) &&
            referencableReifiedRelationships.contains(ax.restrictedRelationUUID)
        }

        val si
        : SortedSet[api.TerminologyBoxStatement]
        = available
          .foldLeft[SortedSet[api.TerminologyBoxStatement]](TreeSet.empty[api.TerminologyBoxStatement]) {
          case (acc, ax) =>
            acc +
            impl.EntityUniversalRestrictionAxiom(
              tbox,
              UUID.fromString(ax.uuid),
              referencableEntities(ax.restrictedDomainUUID),
              referencableEntities(ax.restrictedRangeUUID),
              referencableReifiedRelationships(ax.restrictedRelationUUID))
        }

        TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = TerminologyContext(ri.context.annotationProperties, gj)),
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
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
  (resolver: OMLTablesResolver, queue: Map[UUID, Seq[tables.EntityScalarDataPropertyExistentialRestrictionAxiom]] )
  : Try[(OMLTablesResolver, Seq[tables.EntityScalarDataPropertyExistentialRestrictionAxiom])]
  = {
    queue.foldLeft[Try[(OMLTablesResolver, Map[UUID, Seq[tables.EntityScalarDataPropertyExistentialRestrictionAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)

        val referencableEntities
        : Map[tables.UUID, api.Entity]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.Entity]](Map.empty)(_ ++ _.entities().map(e => e.uuid.toString -> e))

        val referencableEntityScalarDataProperty
        : Map[tables.UUID, api.EntityScalarDataProperty]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.EntityScalarDataProperty]](Map.empty)(_ ++ _.entityScalarDataProperties().map(dr => dr.uuid.toString -> dr))

        val referencableDataRanges
        : Map[tables.UUID, api.DataRange]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.DataRange]](Map.empty)(_ ++ _.dataranges().map(dr => dr.uuid.toString -> dr))

        val (available, remaining) = x.partition { ax =>
          referencableEntities.contains(ax.restrictedEntityUUID) &&
            referencableEntityScalarDataProperty.contains(ax.scalarPropertyUUID) &&
            referencableDataRanges.contains(ax.scalarRestrictionUUID)
        }

        val si
        : SortedSet[api.TerminologyBoxStatement]
        = available
          .foldLeft[SortedSet[api.TerminologyBoxStatement]](TreeSet.empty[api.TerminologyBoxStatement]) {
          case (acc, ax) =>
            acc +
              impl.EntityScalarDataPropertyExistentialRestrictionAxiom(
                tbox,
                UUID.fromString(ax.uuid),
                referencableEntities(ax.restrictedEntityUUID),
                referencableEntityScalarDataProperty(ax.scalarPropertyUUID),
                referencableDataRanges(ax.scalarRestrictionUUID))
        }

        TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = TerminologyContext(ri.context.annotationProperties, gj)),
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
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
  (resolver: OMLTablesResolver, queue: Map[UUID, Seq[tables.EntityScalarDataPropertyParticularRestrictionAxiom]] )
  : Try[(OMLTablesResolver, Seq[tables.EntityScalarDataPropertyParticularRestrictionAxiom])]
  = {
    queue.foldLeft[Try[(OMLTablesResolver, Map[UUID, Seq[tables.EntityScalarDataPropertyParticularRestrictionAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)

        val referencableEntities
        : Map[tables.UUID, api.Entity]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.Entity]](Map.empty)(_ ++ _.entities().map(e => e.uuid.toString -> e))

        val referencableEntityScalarDataProperty
        : Map[tables.UUID, api.EntityScalarDataProperty]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.EntityScalarDataProperty]](Map.empty)(_ ++ _.entityScalarDataProperties().map(dr => dr.uuid.toString -> dr))


        val (available, remaining) = x.partition { ax =>
          referencableEntities.contains(ax.restrictedEntityUUID) &&
            referencableEntityScalarDataProperty.contains(ax.scalarPropertyUUID)
        }

        val si
        : SortedSet[api.TerminologyBoxStatement]
        = available
          .foldLeft[SortedSet[api.TerminologyBoxStatement]](TreeSet.empty[api.TerminologyBoxStatement]) {
          case (acc, ax) =>
            acc +
            impl.EntityScalarDataPropertyParticularRestrictionAxiom(
              tbox,
              UUID.fromString(ax.uuid),
              ax.literalValue,
              referencableEntities(ax.restrictedEntityUUID),
              referencableEntityScalarDataProperty(ax.scalarPropertyUUID))
        }

        TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = TerminologyContext(ri.context.annotationProperties, gj)),
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
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
  (resolver: OMLTablesResolver, queue: Map[UUID, Seq[tables.EntityScalarDataPropertyUniversalRestrictionAxiom]] )
  : Try[(OMLTablesResolver, Seq[tables.EntityScalarDataPropertyUniversalRestrictionAxiom])]
  = {
    queue.foldLeft[Try[(OMLTablesResolver, Map[UUID, Seq[tables.EntityScalarDataPropertyUniversalRestrictionAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)

        val referencableEntities
        : Map[tables.UUID, api.Entity]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.Entity]](Map.empty)(_ ++ _.entities().map(e => e.uuid.toString -> e))

        val referencableEntityScalarDataProperty
        : Map[tables.UUID, api.EntityScalarDataProperty]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.EntityScalarDataProperty]](Map.empty)(_ ++ _.entityScalarDataProperties().map(dr => dr.uuid.toString -> dr))

        val referencableDataRanges
        : Map[tables.UUID, api.DataRange]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.DataRange]](Map.empty)(_ ++ _.dataranges().map(dr => dr.uuid.toString -> dr))

        val (available, remaining) = x.partition { ax =>
          referencableEntities.contains(ax.restrictedEntityUUID) &&
            referencableEntityScalarDataProperty.contains(ax.scalarPropertyUUID) &&
            referencableDataRanges.contains(ax.scalarRestrictionUUID)
        }

        val si
        : SortedSet[api.TerminologyBoxStatement]
        = available
          .foldLeft[SortedSet[api.TerminologyBoxStatement]](TreeSet.empty[api.TerminologyBoxStatement]) {
          case (acc, ax) =>
            acc +
            impl.EntityScalarDataPropertyUniversalRestrictionAxiom(
              tbox,
              UUID.fromString(ax.uuid),
              referencableEntities(ax.restrictedEntityUUID),
              referencableEntityScalarDataProperty(ax.scalarPropertyUUID),
              referencableDataRanges(ax.scalarRestrictionUUID))
        }

        TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = TerminologyContext(ri.context.annotationProperties, gj)),
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
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
  (resolver: OMLTablesResolver, queue: Map[UUID, Seq[tables.AspectSpecializationAxiom]] )
  : Try[(OMLTablesResolver, Seq[tables.AspectSpecializationAxiom])]
  = {
    queue.foldLeft[Try[(OMLTablesResolver, Map[UUID, Seq[tables.AspectSpecializationAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)

        val referencableEntities
        : Map[tables.UUID, api.Entity]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.Entity]](Map.empty)(_ ++ _.entities().map(e => e.uuid.toString -> e))

        val referencableAspects
        : Map[tables.UUID, api.Aspect]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.Aspect]](Map.empty)(_ ++ _.aspects().map(e => e.uuid.toString -> e))

        val (available, remaining) = x.partition { ax =>
          referencableEntities.contains(ax.subEntityUUID) &&
            referencableAspects.contains(ax.superAspectUUID)
        }

        val si
        : SortedSet[api.TerminologyBoxStatement]
        = available
          .foldLeft[SortedSet[api.TerminologyBoxStatement]](TreeSet.empty[api.TerminologyBoxStatement]) {
          case (acc, ax) =>
            acc +
              impl.AspectSpecializationAxiom(
                tbox,
                UUID.fromString(ax.uuid),
                referencableEntities(ax.subEntityUUID),
                referencableAspects(ax.superAspectUUID))
        }

        TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = TerminologyContext(ri.context.annotationProperties, gj)),
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
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
  (resolver: OMLTablesResolver, queue: Map[UUID, Seq[tables.ConceptSpecializationAxiom]] )
  : Try[(OMLTablesResolver, Seq[tables.ConceptSpecializationAxiom])]
  = {
    queue.foldLeft[Try[(OMLTablesResolver, Map[UUID, Seq[tables.ConceptSpecializationAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)

        val referencableConcepts
        : Map[tables.UUID, api.Concept]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.Concept]](Map.empty)(_ ++ _.concepts().map(e => e.uuid.toString -> e))

        val (available, remaining) = x.partition { ax =>
          referencableConcepts.contains(ax.subConceptUUID) &&
            referencableConcepts.contains(ax.superConceptUUID)
        }

        val si
        : SortedSet[api.TerminologyBoxStatement]
        = available
          .foldLeft[SortedSet[api.TerminologyBoxStatement]](TreeSet.empty[api.TerminologyBoxStatement]) {
          case (acc, ax) =>
            acc +
              impl.ConceptSpecializationAxiom(
                tbox,
                UUID.fromString(ax.uuid),
                referencableConcepts(ax.subConceptUUID),
                referencableConcepts(ax.superConceptUUID))
        }

        TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = TerminologyContext(ri.context.annotationProperties, gj)),
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
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
  (resolver: OMLTablesResolver, queue: Map[UUID, Seq[tables.ReifiedRelationshipSpecializationAxiom]] )
  : Try[(OMLTablesResolver, Seq[tables.ReifiedRelationshipSpecializationAxiom])]
  = {
    queue.foldLeft[Try[(OMLTablesResolver, Map[UUID, Seq[tables.ReifiedRelationshipSpecializationAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (guuid, x)) =>
        val gi = ri.context.g
        val tbox = ri.context.nodes(guuid)

        val referencableReifiedRelationships
        : Map[tables.UUID, api.ReifiedRelationship]
        = gi.outerNodeTraverser(gi.get(tbox))
          .foldLeft[Map[tables.UUID, api.ReifiedRelationship]](Map.empty)(_ ++ _.reifiedRelationships().map(e => e.uuid.toString -> e))

        val (available, remaining) = x.partition { ax =>
          referencableReifiedRelationships.contains(ax.subRelationshipUUID) &&
            referencableReifiedRelationships.contains(ax.superRelationshipUUID)
        }

        val si
        : SortedSet[api.TerminologyBoxStatement]
        = available
          .foldLeft[SortedSet[api.TerminologyBoxStatement]](TreeSet.empty[api.TerminologyBoxStatement]) {
          case (acc, ax) =>
            acc +
              impl.ReifiedRelationshipSpecializationAxiom(
                tbox,
                UUID.fromString(ax.uuid),
                referencableReifiedRelationships(ax.subRelationshipUUID),
                referencableReifiedRelationships(ax.superRelationshipUUID))
        }

        TerminologyContext
          .replaceNode(gi, tbox, tbox.withBoxStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = TerminologyContext(ri.context.annotationProperties, gj)),
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
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
  (resolver: OMLTablesResolver, queue: Map[UUID, Seq[tables.RootConceptTaxonomyAxiom]] )
  : Try[(OMLTablesResolver, Seq[tables.RootConceptTaxonomyAxiom])]
  = {
    queue.foldLeft[Try[(OMLTablesResolver, Map[UUID, Seq[tables.RootConceptTaxonomyAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (buuid, x)) =>
        val gi = ri.context.g
        val bundle = ri.context.bundles(buuid)

        val referencableConcepts
        : Map[tables.UUID, api.Concept]
        = gi.outerNodeTraverser(gi.get(bundle))
          .foldLeft[Map[tables.UUID, api.Concept]](Map.empty)(_ ++ _.concepts().map(e => e.uuid.toString -> e))

        val (available, remaining) = x.partition { ax =>
          referencableConcepts.contains(ax.rootUUID)
        }

        val si
        : SortedSet[api.TerminologyBundleStatement]
        = available
          .foldLeft[SortedSet[api.TerminologyBundleStatement]](TreeSet.empty[api.TerminologyBundleStatement]) {
          case (acc, ax) =>
            acc +
              impl.RootConceptTaxonomyAxiom(
                UUID.fromString(ax.uuid),
                bundle,
                referencableConcepts(ax.rootUUID))
        }

        TerminologyContext
          .replaceNode(gi, bundle, bundle.withBundleStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = TerminologyContext(ri.context.annotationProperties, gj)),
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
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
  (resolver: OMLTablesResolver, queue: Map[UUID, Seq[tables.AnonymousConceptTaxonomyAxiom]])
  : Try[(OMLTablesResolver, Seq[tables.AnonymousConceptTaxonomyAxiom])]
  = {
    queue
      .foldLeft[Try[(OMLTablesResolver, Map[UUID, Seq[tables.AnonymousConceptTaxonomyAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (buuid, x)) =>
        val gi = ri.context.g
        val bundle = ri.context.bundles(buuid)

        val referencableConceptTreeDisjunctions
        : Map[tables.UUID, api.ConceptTreeDisjunction]
        = gi.outerNodeTraverser(gi.get(bundle))
          .flatMap {
            case b: api.Bundle => Some(b)
            case _ => None
          }
          .foldLeft[Map[tables.UUID, api.ConceptTreeDisjunction]](Map.empty)(
          _ ++
            _.bundleStatements.flatMap {
                case csd: api.TerminologyBundleStatement with api.ConceptTreeDisjunction =>
                  Some(csd.uuid.toString -> csd)
                case _ =>
                  None
              }
              .toMap)

        val (available, remaining) = x.partition { ax =>
          referencableConceptTreeDisjunctions.contains(ax.disjointTaxonomyParentUUID)
        }

        val si
        : SortedSet[api.TerminologyBundleStatement]
        = available
          .foldLeft[SortedSet[api.TerminologyBundleStatement]](TreeSet.empty[api.TerminologyBundleStatement]) {
          case (acc, ax) =>
            acc +
            impl.AnonymousConceptTaxonomyAxiom(
              UUID.fromString(ax.uuid),
              bundle,
              referencableConceptTreeDisjunctions(ax.disjointTaxonomyParentUUID))
          }

        TerminologyContext
          .replaceNode(gi, bundle, bundle.withBundleStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = TerminologyContext(ri.context.annotationProperties, gj)),
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
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
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
  (resolver: OMLTablesResolver, queue: Map[UUID, Seq[tables.SpecificDisjointConceptAxiom]])
  : Try[(OMLTablesResolver, Seq[tables.SpecificDisjointConceptAxiom])]
  = {
    queue
      .foldLeft[Try[(OMLTablesResolver, Map[UUID, Seq[tables.SpecificDisjointConceptAxiom]], Boolean)]](
      Success(Tuple3(resolver, Map.empty, false))
    ) {
      case (Success(Tuple3(ri, mi, fi)), (buuid, x)) =>
        val gi = ri.context.g
        val bundle = ri.context.bundles(buuid)

        val referencableConcepts
        : Map[tables.UUID, api.Concept]
        = gi.outerNodeTraverser(gi.get(bundle))
          .foldLeft[Map[tables.UUID, api.Concept]](Map.empty)(_ ++ _.concepts().map(e => e.uuid.toString -> e))

        val referencableConceptTreeDisjunctions
        : Map[tables.UUID, api.ConceptTreeDisjunction]
        = gi.outerNodeTraverser(gi.get(bundle))
          .flatMap {
            case b: api.Bundle => Some(b)
            case _ => None
          }
          .foldLeft[Map[tables.UUID, api.ConceptTreeDisjunction]](Map.empty)(
          _ ++
            _.bundleStatements.flatMap {
                case csd: api.TerminologyBundleStatement with api.ConceptTreeDisjunction =>
                  Some(csd.uuid.toString -> csd)
                case _ =>
                  None
              }
              .toMap)

        val (available, remaining) = x.partition { ax =>
          referencableConceptTreeDisjunctions.contains(ax.disjointTaxonomyParentUUID) &&
            referencableConcepts.contains(ax.disjointLeafUUID)
        }

        val si
        : SortedSet[api.TerminologyBundleStatement]
        = available
          .foldLeft[SortedSet[api.TerminologyBundleStatement]](TreeSet.empty[api.TerminologyBundleStatement]) {
          case (acc, ax) =>
            acc +
              impl.SpecificDisjointConceptAxiom(
                UUID.fromString(ax.uuid),
                bundle,
                referencableConcepts(ax.disjointLeafUUID),
                referencableConceptTreeDisjunctions(ax.disjointTaxonomyParentUUID))
        }

        TerminologyContext
          .replaceNode(gi, bundle, bundle.withBundleStatements(si))
          .map { gj =>
            Tuple3(
              ri.copy(context = TerminologyContext(ri.context.annotationProperties, gj)),
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

  type ResolvedAnnotationMap = Map[tables.UUID, Map[api.AnnotationProperty, SortedSet[api.AnnotationEntry]]]
  type AnnotationMapTables = Map[tables.UUID, Map[tables.AnnotationProperty, Seq[tables.AnnotationEntry]]]

  def mergeMapOfSeq[K, V]
  (m1: Map[K, Seq[V]],
   m2: Map[K, Seq[V]])
  : Map[K, Seq[V]]
  = (m1.keySet ++ m2.keySet)
    .map { k =>
      val v1 = m1.getOrElse(k, Seq.empty)
      val v2 = m2.getOrElse(k, Seq.empty)
      k -> (v1 ++ v2)
    }
    .toMap

  def mergeMapOfMapOfSeq[K1, K2, V]
  (mms1: Map[K1, Map[K2, Seq[V]]],
   mms2: Map[K1, Map[K2, Seq[V]]])
  : Map[K1, Map[K2, Seq[V]]]
  = (mms1.keySet ++ mms2.keySet)
    .map { k =>
      val kv1 = mms1.getOrElse(k, Map.empty)
      val kv2 = mms2.getOrElse(k, Map.empty)
      val k2v = mergeMapOfSeq(kv1, kv2)
      k -> k2v
    }
    .toMap

  def mergeMapOfSortedSet[K, V : Ordering]
  (m1: Map[K, SortedSet[V]],
   m2: Map[K, SortedSet[V]])
  : Map[K, SortedSet[V]]
  = (m1.keySet ++ m2.keySet)
    .map { k =>
      val v1 = m1.getOrElse(k, TreeSet.empty[V])
      val v2 = m2.getOrElse(k, TreeSet.empty[V])
      k -> (v1 ++ v2)
    }
    .toMap

  def mergeMapOfMapOfSortedSet[K1, K2, V : Ordering]
  (mms1: Map[K1, Map[K2, SortedSet[V]]],
   mms2: Map[K1, Map[K2, SortedSet[V]]])
  : Map[K1, Map[K2, SortedSet[V]]]
  = (mms1.keySet ++ mms2.keySet)
    .map { k =>
      val kv1 = mms1.getOrElse(k, Map.empty)
      val kv2 = mms2.getOrElse(k, Map.empty)
      val k2v = mergeMapOfSortedSet(kv1, kv2)
      k -> k2v
    }
    .toMap

  def annotationMapC
  (q_u1: (ResolvedAnnotationMap, AnnotationMapTables),
   q_u2: (ResolvedAnnotationMap, AnnotationMapTables))
  : (ResolvedAnnotationMap, AnnotationMapTables)
  = {
    val (q1, u1) = q_u1
    val (q2, u2) = q_u2

    val q = mergeMapOfMapOfSortedSet(q1, q2)
    val u = mergeMapOfMapOfSeq(u1, u2)

    q -> u
  }

  def annotationMapS
  (subjects_by_terminology: Map[tables.UUID, (api.TerminologyBox, Map[tables.UUID, api.TerminologyThing])],
   aps: Map[UUID, api.AnnotationProperty])
  (q_u: (ResolvedAnnotationMap, AnnotationMapTables),
   ap_as: (tables.AnnotationProperty, Seq[tables.AnnotationEntry]))
  : (ResolvedAnnotationMap, AnnotationMapTables)
  = {
    val (q1: ResolvedAnnotationMap, u1: AnnotationMapTables) = q_u
    val (ap: tables.AnnotationProperty, as: Seq[tables.AnnotationEntry]) = ap_as

    // Q: is this a known annotation property?
    aps
      .get(UUID.fromString(ap.uuid))
      .fold[(ResolvedAnnotationMap, AnnotationMapTables)] {

      // A: No, add the annotations to the (unresolved) tables.

      val u2 = as.foldLeft[AnnotationMapTables](u1) { case (ui, a) =>
        val t_pre = ui.getOrElse(a.terminologyUUID, Map.empty)
        val t_upd = t_pre.updated(ap, t_pre.getOrElse(ap, Seq.empty) :+ a)
        ui.updated(a.terminologyUUID, t_upd)
      }

      q1 -> u2

    } { rap: api.AnnotationProperty =>

      // A: Yes, this is a known annotation property
      // However, the annotations may be asserted in unknown terminologies about unknown subjects.

      // First, partition annotations in terms of assertions attributable to a known terminology
      val (t_resolvable: Seq[tables.AnnotationEntry], t_unresolved: Seq[tables.AnnotationEntry]) =
        as.partition(a => subjects_by_terminology.contains(a.terminologyUUID))

      // Second, partition attributable annotations in terms of assertions about known subjects
      val (s_resolvable: Seq[tables.AnnotationEntry], s_unresolved: Seq[tables.AnnotationEntry]) =
        t_resolvable.partition { a =>
          subjects_by_terminology
            .get(a.terminologyUUID)
            .exists(_._2.contains(a.subjectUUID))
        }

      val q2: ResolvedAnnotationMap = s_resolvable.foldLeft[ResolvedAnnotationMap](q1) { case (qi, a) =>
        val annotations_by_prop
        : Map[api.AnnotationProperty, SortedSet[api.AnnotationEntry]]
        = qi.getOrElse(a.terminologyUUID, Map.empty)

        subjects_by_terminology
          .get(a.terminologyUUID)
          .fold[ResolvedAnnotationMap](qi) { case (tbox, subjects) =>
          subjects
            .get(a.subjectUUID)
            .fold[ResolvedAnnotationMap](qi) { subject =>
            val with_a
            : Map[api.AnnotationProperty, SortedSet[api.AnnotationEntry]]
            = annotations_by_prop.updated(
                rap,
                annotations_by_prop.getOrElse(rap, TreeSet.empty[api.AnnotationEntry]) +
                  impl.AnnotationEntry(tbox, subject, a.value))
            qi.updated(a.terminologyUUID, with_a)
          }
        }
      }

      val u2: AnnotationMapTables = u1.map { case (tUUID, annotations_by_prop) =>
          val as = annotations_by_prop.getOrElse(ap, Seq.empty) ++ t_unresolved ++ s_unresolved
          tUUID -> annotations_by_prop.updated(ap, as)
      }

      q2 -> u2
    }
  }

  def mapAnnotationPairs
  (resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
  = {
    val ns = resolver.context.nodes
    val t2everything = ns.map { case (uuid, tbox) =>
        uuid.toString -> (tbox -> tbox.everything().map { e => e.uuid.toString -> e }.toMap)
    }

    val (resolved, remaining)
    = resolver
      .queue
      .annotations.par
      .aggregate[(ResolvedAnnotationMap, AnnotationMapTables)](Map.empty, Map.empty)(
      seqop = annotationMapS(t2everything, resolver.context.annotationProperties),
      combop = annotationMapC)

    val unresolved
    : Map[tables.AnnotationProperty, Seq[tables.AnnotationEntry]]
    = remaining.foldLeft[Map[tables.AnnotationProperty, Seq[tables.AnnotationEntry]]](Map.empty) {
      case (acc, (_, annotations_by_property)) =>
        mergeMapOfSeq(acc, annotations_by_property)
    }
    val r1 = resolver.copy(queue = resolver.queue.copy(annotations = unresolved))
    val r2 = resolved.foldLeft[Try[OMLTablesResolver]](Success(r1)) { case (ri, (tUUID, annotations_by_property)) =>
      ri.flatMap { rj =>
        val tbox = rj.context.nodes(UUID.fromString(tUUID))
        val additions
        : SortedSet[api.AnnotationPropertyTable]
        = annotations_by_property
          .foldLeft[SortedSet[api.AnnotationPropertyTable]](TreeSet.empty[api.AnnotationPropertyTable]) {
          case (acc, (ap, aes)) =>
            acc + impl.AnnotationPropertyTable(ap, aes)
        }

        val rk = TerminologyContext
          .replaceNode(rj.context.g, tbox, tbox.withAnnotations(additions))
          .map { gi =>
            rj.copy(context = TerminologyContext(rj.context.annotationProperties, gi))
          }
        rk
      }
    }
    r2
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
