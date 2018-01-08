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

import ammonite.ops.Path
import org.apache.commons.compress.archivers.zip.ZipFile

import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.imce.oml.tables.{taggedTypes,OMLSpecificationTables}
import scala.collection.immutable.{::, Nil, Map, Seq}
import scala.collection.JavaConversions._
import scala.Boolean
import scala.Predef.{require,ArrowAssoc}

/**
  * Utilities for reading OML Tabular files (`*.omlzip`)
  */
object TableUtilities {

  /**
    * Maps the IRIs of OML Modules in a given OMLSpecificationTables.
    *
    * @param t An OMLSpecificationTables.
    * @return a map of each OML Module IRI to `t`.
    */
  def tableModules(t: OMLSpecificationTables): Map[taggedTypes.IRI, OMLSpecificationTables]
  = t.terminologyGraphs.map(_.iri -> t).toMap ++
    t.bundles.map(_.iri -> t).toMap  ++
    t.descriptionBoxes.map(_.iri-> t).toMap

  /**
    * Extracts the OML ModuleEdges as tuples of IRIs (source to target).
    *
    * @param t An OMLSpecificationTables
    * @return The source/target IRIs of each OML ModuleEdge in `t`.
    */
  def tableEdges(t: OMLSpecificationTables): Seq[(taggedTypes.IRI, taggedTypes.IRI)]
  = {
    val tboxes: Map[taggedTypes.TerminologyBoxUUID, taggedTypes.IRI]
    = t.terminologyGraphs.map(g => g.uuid -> g.iri).toMap ++
      t.bundles.map(g => g.uuid -> g.iri).toMap

    val dboxes: Map[taggedTypes.DescriptionBoxUUID, taggedTypes.IRI]
    = t.descriptionBoxes.map(d => d.uuid -> d.iri).toMap

    t
      .terminologyExtensionAxioms
      .map(e => tboxes(e.tboxUUID) -> e.extendedTerminologyIRI) ++
      t
        .terminologyNestingAxioms
        .map(e => tboxes(e.tboxUUID) -> e.nestingTerminologyIRI) ++
      t
        .bundledTerminologyAxioms
        .map(e => tboxes(e.bundleUUID) -> e.bundledTerminologyIRI) ++
      t
        .descriptionBoxRefinements
        .map(e => dboxes(e.refiningDescriptionBoxUUID) -> e.refinedDescriptionBoxIRI) ++
      t
        .descriptionBoxExtendsClosedWorldDefinitions
        .map(e => dboxes(e.descriptionBoxUUID) -> e.closedWorldDefinitionsIRI)
  }

  @scala.annotation.tailrec
  protected def collectAndPartitionRuleBodySegments
  (seeds: Seq[tables.RuleBodySegment],
   rest: Seq[tables.RuleBodySegment])
  : (Seq[tables.RuleBodySegment], Seq[tables.RuleBodySegment])
  = if (seeds.isEmpty)
    (Seq.empty, rest)
  else if (rest.isEmpty)
    (seeds, Seq.empty)
  else {
    val (more, other) = rest.partition { r =>
      seeds.exists(s => r.previousSegmentUUID.contains(s.uuid))
    }
    if (more.isEmpty)
      (seeds, other)
    else
      collectAndPartitionRuleBodySegments(seeds ++ more, other)
  }

  @scala.annotation.tailrec
  protected def collectAndPartitionRestrictionStructuredDataPropertyTuples
  (seeds: Seq[tables.taggedTypes.RestrictionStructuredDataPropertyContextUUID],
   acc: Seq[tables.RestrictionStructuredDataPropertyTuple],
   rest: Seq[tables.RestrictionStructuredDataPropertyTuple])
  : (Seq[tables.RestrictionStructuredDataPropertyTuple],
     Seq[tables.RestrictionStructuredDataPropertyTuple])
  = if (seeds.isEmpty)
    (acc, rest)
  else if (rest.isEmpty)
    (acc, Seq.empty)
  else {
    val (more, other) = rest.partition { r => seeds.contains(r.structuredDataPropertyContextUUID) }
    collectAndPartitionRestrictionStructuredDataPropertyTuples(
      seeds ++ more.map(_.uuid),
      acc ++ more,
      other)
  }

  @scala.annotation.tailrec
  protected def collectAndPartitionConceptTreeDisjunctions
  (seeds: Seq[tables.taggedTypes.ConceptTreeDisjunctionUUID],
   acc: Seq[tables.AnonymousConceptUnionAxiom],
   rest: Seq[tables.AnonymousConceptUnionAxiom])
  : (Seq[tables.AnonymousConceptUnionAxiom],
    Seq[tables.AnonymousConceptUnionAxiom])
  = if (seeds.isEmpty)
    (acc, rest)
  else if (rest.isEmpty)
    (acc, Seq.empty)
  else {
    val (more, other) = rest.partition { r => seeds.contains(r.disjointTaxonomyParentUUID) }
    collectAndPartitionConceptTreeDisjunctions(
      seeds ++ more.map(_.uuid),
      acc ++ more,
      other)
  }

  @scala.annotation.tailrec
  protected def collectAndPartitionStructuredDataPropertyTuples
  (seeds: Seq[tables.taggedTypes.SingletonInstanceStructuredDataPropertyContextUUID],
   acc: Seq[tables.StructuredDataPropertyTuple],
   rest: Seq[tables.StructuredDataPropertyTuple])
  : (Seq[tables.StructuredDataPropertyTuple],
    Seq[tables.StructuredDataPropertyTuple])
  = if (seeds.isEmpty)
    (acc, rest)
  else if (rest.isEmpty)
    (acc, Seq.empty)
  else {
    val (more, other) = rest.partition { r => seeds.contains(r.structuredDataPropertyContextUUID) }
    collectAndPartitionStructuredDataPropertyTuples(
      seeds ++ more.map(_.uuid),
      acc ++ more,
      other)
  }

  protected def partitionTerminologyGraph
  (g: tables.TerminologyGraph, ts: OMLSpecificationTables)
  : (tables.taggedTypes.IRI, OMLSpecificationTables, OMLSpecificationTables)
  = {
    val annotationProperties
    = ts.annotationProperties.partition(_.moduleUUID == g.uuid)

    val aspects
    = ts.aspects.partition(_.tboxUUID == g.uuid)
    val concepts
    = ts.concepts.partition(_.tboxUUID == g.uuid)

    val scalars
    = ts.scalars.partition(_.tboxUUID == g.uuid)
    val structures
    = ts.structures.partition(_.tboxUUID == g.uuid)

    val conceptDesignationTerminologyAxioms
    = ts.conceptDesignationTerminologyAxioms.partition(_.tboxUUID == g.uuid)
    val terminologyExtensionAxioms
    = ts.terminologyExtensionAxioms.partition(_.tboxUUID == g.uuid)
    val terminologyNestingAxioms
    = ts.terminologyNestingAxioms.partition(_.tboxUUID == g.uuid)

    val binaryScalarRestrictions
    = ts.binaryScalarRestrictions.partition(_.tboxUUID == g.uuid)
    val iriScalarRestrictions
    = ts.iriScalarRestrictions.partition(_.tboxUUID == g.uuid)
    val numericScalarRestrictions
    = ts.numericScalarRestrictions.partition(_.tboxUUID == g.uuid)
    val plainLiteralScalarRestrictions
    = ts.plainLiteralScalarRestrictions.partition(_.tboxUUID == g.uuid)
    val scalarOneOfRestrictions
    = ts.scalarOneOfRestrictions.partition(_.tboxUUID == g.uuid)
    val scalarOneOfLiteralAxioms
    = ts.scalarOneOfLiteralAxioms.partition(_.tboxUUID == g.uuid)
    val stringScalarRestrictions
    = ts.stringScalarRestrictions.partition(_.tboxUUID == g.uuid)
    val synonymScalarRestrictions
    = ts.synonymScalarRestrictions.partition(_.tboxUUID == g.uuid)
    val timeScalarRestrictions
    = ts.timeScalarRestrictions.partition(_.tboxUUID == g.uuid)

    val entityScalarDataProperties
    = ts.entityScalarDataProperties.partition(_.tboxUUID == g.uuid)
    val entityStructuredDataProperties
    = ts.entityStructuredDataProperties.partition(_.tboxUUID == g.uuid)
    val scalarDataProperties
    = ts.scalarDataProperties.partition(_.tboxUUID == g.uuid)
    val structuredDataProperties
    = ts.structuredDataProperties.partition(_.tboxUUID == g.uuid)

    val reifiedRelationships
    = ts.reifiedRelationships.partition(_.tboxUUID == g.uuid)
    val unreifiedRelationships
    = ts.unreifiedRelationships.partition(_.tboxUUID == g.uuid)

    val chainRules
    = ts.chainRules.partition(_.tboxUUID == g.uuid)
    val chainRulesUUIDs
    = chainRules._1.map(_.uuid)

    val (headBodySegments, tailBodySegments)
    = ts.ruleBodySegments.partition(_.ruleUUID.nonEmpty)

    val ruleBodySegments
    = headBodySegments.partition(_.ruleUUID.fold[Boolean](false) { uuid => chainRulesUUIDs.contains(uuid) })

    val otherBodySegments
    = collectAndPartitionRuleBodySegments(ruleBodySegments._1, tailBodySegments)

    val bodySegments = ruleBodySegments._1 ++ otherBodySegments._1
    val bodySegmentsUUIDs = bodySegments.map(_.uuid)

    val aspectPredicates
    = ts.aspectPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))
    val conceptPredicates
    = ts.conceptPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))
    val reifiedRelationshipPredicates
    = ts.reifiedRelationshipPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))
    val reifiedRelationshipPropertyPredicates
    = ts.reifiedRelationshipPropertyPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))
    val reifiedRelationshipSourcePropertyPredicates
    = ts.reifiedRelationshipSourcePropertyPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))
    val reifiedRelationshipTargetPropertyPredicates
    = ts.reifiedRelationshipTargetPropertyPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))
    val unreifiedRelationshipPropertyPredicates
    = ts.unreifiedRelationshipPropertyPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))
    val reifiedRelationshipInversePropertyPredicates
    = ts.reifiedRelationshipInversePropertyPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))
    val reifiedRelationshipSourceInversePropertyPredicates
    = ts.reifiedRelationshipSourceInversePropertyPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))
    val reifiedRelationshipTargetInversePropertyPredicates
    = ts.reifiedRelationshipTargetInversePropertyPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))
    val unreifiedRelationshipInversePropertyPredicates
    = ts.unreifiedRelationshipInversePropertyPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))

    val entityExistentialRestrictionAxioms
    = ts.entityExistentialRestrictionAxioms.partition(_.tboxUUID == g.uuid)
    val entityUniversalRestrictionAxioms
    = ts.entityUniversalRestrictionAxioms.partition(_.tboxUUID == g.uuid)

    val entityScalarDataPropertyExistentialRestrictionAxioms
    = ts.entityScalarDataPropertyExistentialRestrictionAxioms.partition(_.tboxUUID == g.uuid)
    val entityScalarDataPropertyParticularRestrictionAxioms
    = ts.entityScalarDataPropertyParticularRestrictionAxioms.partition(_.tboxUUID == g.uuid)
    val entityScalarDataPropertyUniversalRestrictionAxioms
    = ts.entityScalarDataPropertyUniversalRestrictionAxioms.partition(_.tboxUUID == g.uuid)
    val entityStructuredDataPropertyParticularRestrictionAxioms
    = ts.entityStructuredDataPropertyParticularRestrictionAxioms.partition(_.tboxUUID == g.uuid)

    val restrictionStructuredDataPropertyTuples
    = collectAndPartitionRestrictionStructuredDataPropertyTuples(
      entityStructuredDataPropertyParticularRestrictionAxioms._1.map(_.uuid),
      Seq.empty,
      ts.restrictionStructuredDataPropertyTuples)

    val restrictionStructuredDataPropertyContextUUIDs
    = entityStructuredDataPropertyParticularRestrictionAxioms._1.map(_.uuid) ++
      restrictionStructuredDataPropertyTuples._1.map(_.uuid)

    val restrictionScalarDataPropertyValues
    = ts.restrictionScalarDataPropertyValues.partition { v =>
      restrictionStructuredDataPropertyContextUUIDs.contains(v.structuredDataPropertyContextUUID)
    }

    val aspectSpecializationAxioms
    = ts.aspectSpecializationAxioms.partition(_.tboxUUID == g.uuid)
    val conceptSpecializationAxioms
    = ts.conceptSpecializationAxioms.partition(_.tboxUUID == g.uuid)
    val reifiedRelationshipSpecializationAxioms
    = ts.reifiedRelationshipSpecializationAxioms.partition(_.tboxUUID == g.uuid)

    val subDataPropertyOfAxioms
    = ts.subDataPropertyOfAxioms.partition(_.tboxUUID == g.uuid)
    val subObjectPropertyOfAxioms
    = ts.subObjectPropertyOfAxioms.partition(_.tboxUUID == g.uuid)

    val allLogicalUUIDs
    : Seq[tables.taggedTypes.LogicalElementUUID]
    = Seq.empty[tables.taggedTypes.LogicalElementUUID] ++
      Seq(g.uuid) ++
      aspects._1.map(_.uuid) ++
      concepts._1.map(_.uuid) ++
      scalars._1.map(_.uuid) ++
      structures._1.map(_.uuid) ++
      conceptDesignationTerminologyAxioms._1.map(_.uuid) ++
      terminologyExtensionAxioms._1.map(_.uuid) ++
      terminologyNestingAxioms._1.map(_.uuid) ++
      binaryScalarRestrictions._1.map(_.uuid) ++
      iriScalarRestrictions._1.map(_.uuid) ++
      numericScalarRestrictions._1.map(_.uuid) ++
      plainLiteralScalarRestrictions._1.map(_.uuid) ++
      scalarOneOfRestrictions._1.map(_.uuid) ++
      scalarOneOfLiteralAxioms._1.map(_.uuid) ++
      stringScalarRestrictions._1.map(_.uuid) ++
      synonymScalarRestrictions._1.map(_.uuid) ++
      timeScalarRestrictions._1.map(_.uuid) ++
      entityScalarDataProperties._1.map(_.uuid) ++
      entityStructuredDataProperties._1.map(_.uuid) ++
      scalarDataProperties._1.map(_.uuid) ++
      structuredDataProperties._1.map(_.uuid) ++
      reifiedRelationships._1.map(_.uuid) ++
      unreifiedRelationships._1.map(_.uuid) ++
      chainRules._1.map(_.uuid) ++
      bodySegments.map(_.uuid) ++
      aspectPredicates._1.map(_.uuid) ++
      conceptPredicates._1.map(_.uuid) ++
      reifiedRelationshipPredicates._1.map(_.uuid) ++
      reifiedRelationshipPropertyPredicates._1.map(_.uuid) ++
      reifiedRelationshipSourcePropertyPredicates._1.map(_.uuid) ++
      reifiedRelationshipTargetPropertyPredicates._1.map(_.uuid) ++
      unreifiedRelationshipPropertyPredicates._1.map(_.uuid) ++
      reifiedRelationshipInversePropertyPredicates._1.map(_.uuid) ++
      reifiedRelationshipSourceInversePropertyPredicates._1.map(_.uuid) ++
      reifiedRelationshipTargetInversePropertyPredicates._1.map(_.uuid) ++
      unreifiedRelationshipInversePropertyPredicates._1.map(_.uuid) ++
      entityExistentialRestrictionAxioms._1.map(_.uuid) ++
      entityUniversalRestrictionAxioms._1.map(_.uuid) ++
      entityScalarDataPropertyExistentialRestrictionAxioms._1.map(_.uuid) ++
      entityScalarDataPropertyParticularRestrictionAxioms._1.map(_.uuid) ++
      entityScalarDataPropertyUniversalRestrictionAxioms._1.map(_.uuid) ++
      entityStructuredDataPropertyParticularRestrictionAxioms._1.map(_.uuid) ++
      restrictionStructuredDataPropertyTuples._1.map(_.uuid) ++
      restrictionScalarDataPropertyValues._1.map(_.uuid) ++
      aspectSpecializationAxioms._1.map(_.uuid) ++
      conceptSpecializationAxioms._1.map(_.uuid) ++
      reifiedRelationshipSpecializationAxioms._1.map(_.uuid) ++
      subDataPropertyOfAxioms._1.map(_.uuid) ++
      subObjectPropertyOfAxioms._1.map(_.uuid)

    val annotationPropertyValues
    = ts.annotationPropertyValues.partition(apv => allLogicalUUIDs.contains(apv.subjectUUID))

    val tg = OMLSpecificationTables(
      terminologyGraphs = Seq(g),

      annotationProperties = annotationProperties._1,

      aspects = aspects._1,
      concepts = concepts._1,

      scalars = scalars._1,
      structures = structures._1,

      conceptDesignationTerminologyAxioms = conceptDesignationTerminologyAxioms._1,
      terminologyExtensionAxioms = terminologyExtensionAxioms._1,
      terminologyNestingAxioms = terminologyNestingAxioms._1,

      binaryScalarRestrictions = binaryScalarRestrictions._1,
      iriScalarRestrictions = iriScalarRestrictions._1,
      numericScalarRestrictions = numericScalarRestrictions._1,
      plainLiteralScalarRestrictions = plainLiteralScalarRestrictions._1,
      scalarOneOfRestrictions = scalarOneOfRestrictions._1,
      scalarOneOfLiteralAxioms = scalarOneOfLiteralAxioms._1,
      stringScalarRestrictions = stringScalarRestrictions._1,
      synonymScalarRestrictions = synonymScalarRestrictions._1,
      timeScalarRestrictions = timeScalarRestrictions._1,

      entityScalarDataProperties = entityScalarDataProperties._1,
      entityStructuredDataProperties = entityStructuredDataProperties._1,
      scalarDataProperties = scalarDataProperties._1,
      structuredDataProperties = structuredDataProperties._1,

      reifiedRelationships = reifiedRelationships._1,
      unreifiedRelationships = unreifiedRelationships._1,

      chainRules = chainRules._1,
      ruleBodySegments = bodySegments,

      aspectPredicates = aspectPredicates._1,
      conceptPredicates = conceptPredicates._1,
      reifiedRelationshipPredicates = reifiedRelationshipPredicates._1,
      reifiedRelationshipPropertyPredicates = reifiedRelationshipPropertyPredicates._1,
      reifiedRelationshipSourcePropertyPredicates = reifiedRelationshipSourcePropertyPredicates._1,
      reifiedRelationshipTargetPropertyPredicates = reifiedRelationshipTargetPropertyPredicates._1,
      unreifiedRelationshipPropertyPredicates = unreifiedRelationshipPropertyPredicates._1,
      reifiedRelationshipInversePropertyPredicates = reifiedRelationshipInversePropertyPredicates._1,
      reifiedRelationshipSourceInversePropertyPredicates = reifiedRelationshipSourceInversePropertyPredicates._1,
      reifiedRelationshipTargetInversePropertyPredicates = reifiedRelationshipTargetInversePropertyPredicates._1,
      unreifiedRelationshipInversePropertyPredicates = unreifiedRelationshipInversePropertyPredicates._1,

      entityExistentialRestrictionAxioms = entityExistentialRestrictionAxioms._1,
      entityUniversalRestrictionAxioms = entityUniversalRestrictionAxioms._1,

      entityScalarDataPropertyExistentialRestrictionAxioms = entityScalarDataPropertyExistentialRestrictionAxioms._1,
      entityScalarDataPropertyParticularRestrictionAxioms = entityScalarDataPropertyParticularRestrictionAxioms._1,
      entityScalarDataPropertyUniversalRestrictionAxioms = entityScalarDataPropertyUniversalRestrictionAxioms._1,
      entityStructuredDataPropertyParticularRestrictionAxioms = entityStructuredDataPropertyParticularRestrictionAxioms._1,

      restrictionStructuredDataPropertyTuples = restrictionStructuredDataPropertyTuples._1,
      restrictionScalarDataPropertyValues = restrictionScalarDataPropertyValues._1,

      aspectSpecializationAxioms = aspectSpecializationAxioms._1,
      conceptSpecializationAxioms = conceptSpecializationAxioms._1,
      reifiedRelationshipSpecializationAxioms = reifiedRelationshipSpecializationAxioms._1,

      subDataPropertyOfAxioms = subDataPropertyOfAxioms._1,
      subObjectPropertyOfAxioms = subObjectPropertyOfAxioms._1,

      annotationPropertyValues = annotationPropertyValues._1
    )

    val og = ts.copy(
      terminologyGraphs = ts.terminologyGraphs.filter(_ != g),

      annotationProperties = annotationProperties._2,

      aspects = aspects._2,
      concepts = concepts._2,

      scalars = scalars._2,
      structures = structures._2,

      conceptDesignationTerminologyAxioms = conceptDesignationTerminologyAxioms._2,
      terminologyExtensionAxioms = terminologyExtensionAxioms._2,
      terminologyNestingAxioms = terminologyNestingAxioms._2,

      binaryScalarRestrictions = binaryScalarRestrictions._2,
      iriScalarRestrictions = iriScalarRestrictions._2,
      numericScalarRestrictions = numericScalarRestrictions._2,
      plainLiteralScalarRestrictions = plainLiteralScalarRestrictions._2,
      scalarOneOfRestrictions = scalarOneOfRestrictions._2,
      scalarOneOfLiteralAxioms = scalarOneOfLiteralAxioms._2,
      stringScalarRestrictions = stringScalarRestrictions._2,
      synonymScalarRestrictions = synonymScalarRestrictions._2,
      timeScalarRestrictions = timeScalarRestrictions._2,

      entityScalarDataProperties = entityScalarDataProperties._2,
      entityStructuredDataProperties = entityStructuredDataProperties._2,
      scalarDataProperties = scalarDataProperties._2,
      structuredDataProperties = structuredDataProperties._2,

      reifiedRelationships = reifiedRelationships._2,
      unreifiedRelationships = unreifiedRelationships._2,

      chainRules = chainRules._2,
      ruleBodySegments = ruleBodySegments._2 ++ otherBodySegments._2,

      aspectPredicates = aspectPredicates._2,
      conceptPredicates = conceptPredicates._2,
      reifiedRelationshipPredicates = reifiedRelationshipPredicates._2,
      reifiedRelationshipPropertyPredicates = reifiedRelationshipPropertyPredicates._2,
      reifiedRelationshipSourcePropertyPredicates = reifiedRelationshipSourcePropertyPredicates._2,
      reifiedRelationshipTargetPropertyPredicates = reifiedRelationshipTargetPropertyPredicates._2,
      unreifiedRelationshipPropertyPredicates = unreifiedRelationshipPropertyPredicates._2,
      reifiedRelationshipInversePropertyPredicates = reifiedRelationshipInversePropertyPredicates._2,
      reifiedRelationshipSourceInversePropertyPredicates = reifiedRelationshipSourceInversePropertyPredicates._2,
      reifiedRelationshipTargetInversePropertyPredicates = reifiedRelationshipTargetInversePropertyPredicates._2,
      unreifiedRelationshipInversePropertyPredicates = unreifiedRelationshipInversePropertyPredicates._2,

      entityExistentialRestrictionAxioms = entityExistentialRestrictionAxioms._2,
      entityUniversalRestrictionAxioms = entityUniversalRestrictionAxioms._2,

      entityScalarDataPropertyExistentialRestrictionAxioms = entityScalarDataPropertyExistentialRestrictionAxioms._2,
      entityScalarDataPropertyParticularRestrictionAxioms = entityScalarDataPropertyParticularRestrictionAxioms._2,
      entityScalarDataPropertyUniversalRestrictionAxioms = entityScalarDataPropertyUniversalRestrictionAxioms._2,
      entityStructuredDataPropertyParticularRestrictionAxioms = entityStructuredDataPropertyParticularRestrictionAxioms._2,

      restrictionStructuredDataPropertyTuples = restrictionStructuredDataPropertyTuples._2,
      restrictionScalarDataPropertyValues = restrictionScalarDataPropertyValues._2,

      aspectSpecializationAxioms = aspectSpecializationAxioms._2,
      conceptSpecializationAxioms = conceptSpecializationAxioms._2,
      reifiedRelationshipSpecializationAxioms = reifiedRelationshipSpecializationAxioms._2,

      subDataPropertyOfAxioms = subDataPropertyOfAxioms._2,
      subObjectPropertyOfAxioms = subObjectPropertyOfAxioms._2,

      annotationPropertyValues = annotationPropertyValues._2
    )

    (g.iri, tg, og)
  }

  protected def partitionBundle
  (b: tables.Bundle, ts: OMLSpecificationTables)
  : (tables.taggedTypes.IRI, OMLSpecificationTables, OMLSpecificationTables)
  = {
    val annotationProperties
    = ts.annotationProperties.partition(_.moduleUUID == b.uuid)

    val aspects
    = ts.aspects.partition(_.tboxUUID == b.uuid)
    val concepts
    = ts.concepts.partition(_.tboxUUID == b.uuid)

    val scalars
    = ts.scalars.partition(_.tboxUUID == b.uuid)
    val structures
    = ts.structures.partition(_.tboxUUID == b.uuid)

    val conceptDesignationTerminologyAxioms
    = ts.conceptDesignationTerminologyAxioms.partition(_.tboxUUID == b.uuid)
    val terminologyExtensionAxioms
    = ts.terminologyExtensionAxioms.partition(_.tboxUUID == b.uuid)
    val terminologyNestingAxioms
    = ts.terminologyNestingAxioms.partition(_.tboxUUID == b.uuid)
    val bundledTerminologyAxioms
    = ts.bundledTerminologyAxioms.partition(_.bundleUUID == b.uuid)

    val binaryScalarRestrictions
    = ts.binaryScalarRestrictions.partition(_.tboxUUID == b.uuid)
    val iriScalarRestrictions
    = ts.iriScalarRestrictions.partition(_.tboxUUID == b.uuid)
    val numericScalarRestrictions
    = ts.numericScalarRestrictions.partition(_.tboxUUID == b.uuid)
    val plainLiteralScalarRestrictions
    = ts.plainLiteralScalarRestrictions.partition(_.tboxUUID == b.uuid)
    val scalarOneOfRestrictions
    = ts.scalarOneOfRestrictions.partition(_.tboxUUID == b.uuid)
    val scalarOneOfLiteralAxioms
    = ts.scalarOneOfLiteralAxioms.partition(_.tboxUUID == b.uuid)
    val stringScalarRestrictions
    = ts.stringScalarRestrictions.partition(_.tboxUUID == b.uuid)
    val synonymScalarRestrictions
    = ts.synonymScalarRestrictions.partition(_.tboxUUID == b.uuid)
    val timeScalarRestrictions
    = ts.timeScalarRestrictions.partition(_.tboxUUID == b.uuid)

    val entityScalarDataProperties
    = ts.entityScalarDataProperties.partition(_.tboxUUID == b.uuid)
    val entityStructuredDataProperties
    = ts.entityStructuredDataProperties.partition(_.tboxUUID == b.uuid)
    val scalarDataProperties
    = ts.scalarDataProperties.partition(_.tboxUUID == b.uuid)
    val structuredDataProperties
    = ts.structuredDataProperties.partition(_.tboxUUID == b.uuid)

    val reifiedRelationships
    = ts.reifiedRelationships.partition(_.tboxUUID == b.uuid)
    val unreifiedRelationships
    = ts.unreifiedRelationships.partition(_.tboxUUID == b.uuid)

    val chainRules
    = ts.chainRules.partition(_.tboxUUID == b.uuid)
    val chainRulesUUIDs
    = chainRules._1.map(_.uuid)

    val (headBodySegments, tailBodySegments)
    = ts.ruleBodySegments.partition(_.ruleUUID.nonEmpty)

    val ruleBodySegments
    = headBodySegments.partition(_.ruleUUID.fold[Boolean](false) { uuid => chainRulesUUIDs.contains(uuid) })

    val otherBodySegments
    = collectAndPartitionRuleBodySegments(ruleBodySegments._1, tailBodySegments)

    val bodySegments = ruleBodySegments._1 ++ otherBodySegments._1
    val bodySegmentsUUIDs = bodySegments.map(_.uuid)

    val aspectPredicates
    = ts.aspectPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))
    val conceptPredicates
    = ts.conceptPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))
    val reifiedRelationshipPredicates
    = ts.reifiedRelationshipPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))
    val reifiedRelationshipPropertyPredicates
    = ts.reifiedRelationshipPropertyPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))
    val reifiedRelationshipSourcePropertyPredicates
    = ts.reifiedRelationshipSourcePropertyPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))
    val reifiedRelationshipTargetPropertyPredicates
    = ts.reifiedRelationshipTargetPropertyPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))
    val unreifiedRelationshipPropertyPredicates
    = ts.unreifiedRelationshipPropertyPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))
    val reifiedRelationshipInversePropertyPredicates
    = ts.reifiedRelationshipInversePropertyPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))
    val reifiedRelationshipSourceInversePropertyPredicates
    = ts.reifiedRelationshipSourceInversePropertyPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))
    val reifiedRelationshipTargetInversePropertyPredicates
    = ts.reifiedRelationshipTargetInversePropertyPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))
    val unreifiedRelationshipInversePropertyPredicates
    = ts.unreifiedRelationshipInversePropertyPredicates.partition(p => bodySegmentsUUIDs.contains(p.bodySegmentUUID))

    val entityExistentialRestrictionAxioms
    = ts.entityExistentialRestrictionAxioms.partition(_.tboxUUID == b.uuid)
    val entityUniversalRestrictionAxioms
    = ts.entityUniversalRestrictionAxioms.partition(_.tboxUUID == b.uuid)

    val entityScalarDataPropertyExistentialRestrictionAxioms
    = ts.entityScalarDataPropertyExistentialRestrictionAxioms.partition(_.tboxUUID == b.uuid)
    val entityScalarDataPropertyParticularRestrictionAxioms
    = ts.entityScalarDataPropertyParticularRestrictionAxioms.partition(_.tboxUUID == b.uuid)
    val entityScalarDataPropertyUniversalRestrictionAxioms
    = ts.entityScalarDataPropertyUniversalRestrictionAxioms.partition(_.tboxUUID == b.uuid)
    val entityStructuredDataPropertyParticularRestrictionAxioms
    = ts.entityStructuredDataPropertyParticularRestrictionAxioms.partition(_.tboxUUID == b.uuid)

    val restrictionStructuredDataPropertyTuples
    = collectAndPartitionRestrictionStructuredDataPropertyTuples(
      entityStructuredDataPropertyParticularRestrictionAxioms._1.map(_.uuid),
      Seq.empty,
      ts.restrictionStructuredDataPropertyTuples)

    val restrictionStructuredDataPropertyContextUUIDs
    = entityStructuredDataPropertyParticularRestrictionAxioms._1.map(_.uuid) ++
      restrictionStructuredDataPropertyTuples._1.map(_.uuid)

    val restrictionScalarDataPropertyValues
    = ts.restrictionScalarDataPropertyValues.partition { v =>
      restrictionStructuredDataPropertyContextUUIDs.contains(v.structuredDataPropertyContextUUID)
    }

    val aspectSpecializationAxioms
    = ts.aspectSpecializationAxioms.partition(_.tboxUUID == b.uuid)
    val conceptSpecializationAxioms
    = ts.conceptSpecializationAxioms.partition(_.tboxUUID == b.uuid)
    val reifiedRelationshipSpecializationAxioms
    = ts.reifiedRelationshipSpecializationAxioms.partition(_.tboxUUID == b.uuid)

    val subDataPropertyOfAxioms
    = ts.subDataPropertyOfAxioms.partition(_.tboxUUID == b.uuid)
    val subObjectPropertyOfAxioms
    = ts.subObjectPropertyOfAxioms.partition(_.tboxUUID == b.uuid)

    val rootConceptTaxonomyAxioms
    = ts.rootConceptTaxonomyAxioms.partition(_.bundleUUID == b.uuid)

    val anonymousConceptUnionAxioms
    = collectAndPartitionConceptTreeDisjunctions(
      rootConceptTaxonomyAxioms._1.map(_.uuid),
      Seq.empty,
      ts.anonymousConceptUnionAxioms)

    val conceptTreeDisjunctionUUIDs
    = rootConceptTaxonomyAxioms._1.map(_.uuid) ++
      anonymousConceptUnionAxioms._1.map(_.uuid)

    val specificDisjointConceptAxioms
    = ts.specificDisjointConceptAxioms.partition { s =>
      conceptTreeDisjunctionUUIDs.contains(s.disjointTaxonomyParentUUID)
    }

    val allLogicalUUIDs
    : Seq[tables.taggedTypes.LogicalElementUUID]
    = Seq.empty[tables.taggedTypes.LogicalElementUUID] ++
      Seq(b.uuid) ++
      aspects._1.map(_.uuid) ++
      concepts._1.map(_.uuid) ++
      scalars._1.map(_.uuid) ++
      structures._1.map(_.uuid) ++
      conceptDesignationTerminologyAxioms._1.map(_.uuid) ++
      terminologyExtensionAxioms._1.map(_.uuid) ++
      terminologyNestingAxioms._1.map(_.uuid) ++
      bundledTerminologyAxioms._1.map(_.uuid) ++
      binaryScalarRestrictions._1.map(_.uuid) ++
      iriScalarRestrictions._1.map(_.uuid) ++
      numericScalarRestrictions._1.map(_.uuid) ++
      plainLiteralScalarRestrictions._1.map(_.uuid) ++
      scalarOneOfRestrictions._1.map(_.uuid) ++
      scalarOneOfLiteralAxioms._1.map(_.uuid) ++
      stringScalarRestrictions._1.map(_.uuid) ++
      timeScalarRestrictions._1.map(_.uuid) ++
      entityScalarDataProperties._1.map(_.uuid) ++
      entityStructuredDataProperties._1.map(_.uuid) ++
      scalarDataProperties._1.map(_.uuid) ++
      structuredDataProperties._1.map(_.uuid) ++
      reifiedRelationships._1.map(_.uuid) ++
      unreifiedRelationships._1.map(_.uuid) ++
      chainRules._1.map(_.uuid) ++
      bodySegments.map(_.uuid) ++
      aspectPredicates._1.map(_.uuid) ++
      conceptPredicates._1.map(_.uuid) ++
      reifiedRelationshipPredicates._1.map(_.uuid) ++
      reifiedRelationshipPropertyPredicates._1.map(_.uuid) ++
      reifiedRelationshipSourcePropertyPredicates._1.map(_.uuid) ++
      reifiedRelationshipTargetPropertyPredicates._1.map(_.uuid) ++
      unreifiedRelationshipPropertyPredicates._1.map(_.uuid) ++
      reifiedRelationshipInversePropertyPredicates._1.map(_.uuid) ++
      reifiedRelationshipSourceInversePropertyPredicates._1.map(_.uuid) ++
      reifiedRelationshipTargetInversePropertyPredicates._1.map(_.uuid) ++
      unreifiedRelationshipInversePropertyPredicates._1.map(_.uuid) ++
      entityExistentialRestrictionAxioms._1.map(_.uuid) ++
      entityUniversalRestrictionAxioms._1.map(_.uuid) ++
      entityScalarDataPropertyExistentialRestrictionAxioms._1.map(_.uuid) ++
      entityScalarDataPropertyParticularRestrictionAxioms._1.map(_.uuid) ++
      entityScalarDataPropertyUniversalRestrictionAxioms._1.map(_.uuid) ++
      entityStructuredDataPropertyParticularRestrictionAxioms._1.map(_.uuid) ++
      restrictionStructuredDataPropertyTuples._1.map(_.uuid) ++
      restrictionScalarDataPropertyValues._1.map(_.uuid) ++
      aspectSpecializationAxioms._1.map(_.uuid) ++
      conceptSpecializationAxioms._1.map(_.uuid) ++
      reifiedRelationshipSpecializationAxioms._1.map(_.uuid) ++
      subDataPropertyOfAxioms._1.map(_.uuid) ++
      subObjectPropertyOfAxioms._1.map(_.uuid) ++
      rootConceptTaxonomyAxioms._1.map(_.uuid) ++
      anonymousConceptUnionAxioms._1.map(_.uuid) ++
      specificDisjointConceptAxioms._1.map(_.uuid)

    val annotationPropertyValues
    = ts.annotationPropertyValues.partition(apv => allLogicalUUIDs.contains(apv.subjectUUID))

    val tb = OMLSpecificationTables(
      bundles = Seq(b),

      annotationProperties = annotationProperties._1,

      aspects = aspects._1,
      concepts = concepts._1,

      scalars = scalars._1,
      structures = structures._1,

      conceptDesignationTerminologyAxioms = conceptDesignationTerminologyAxioms._1,
      terminologyExtensionAxioms = terminologyExtensionAxioms._1,
      terminologyNestingAxioms = terminologyNestingAxioms._1,
      bundledTerminologyAxioms = bundledTerminologyAxioms._1,

      binaryScalarRestrictions = binaryScalarRestrictions._1,
      iriScalarRestrictions = iriScalarRestrictions._1,
      numericScalarRestrictions = numericScalarRestrictions._1,
      plainLiteralScalarRestrictions = plainLiteralScalarRestrictions._1,
      scalarOneOfRestrictions = scalarOneOfRestrictions._1,
      scalarOneOfLiteralAxioms = scalarOneOfLiteralAxioms._1,
      stringScalarRestrictions = stringScalarRestrictions._1,
      timeScalarRestrictions = timeScalarRestrictions._1,

      entityScalarDataProperties = entityScalarDataProperties._1,
      entityStructuredDataProperties = entityStructuredDataProperties._1,
      scalarDataProperties = scalarDataProperties._1,
      structuredDataProperties = structuredDataProperties._1,

      reifiedRelationships = reifiedRelationships._1,
      unreifiedRelationships = unreifiedRelationships._1,

      chainRules = chainRules._1,
      ruleBodySegments = bodySegments,

      aspectPredicates = aspectPredicates._1,
      conceptPredicates = conceptPredicates._1,
      reifiedRelationshipPredicates = reifiedRelationshipPredicates._1,
      reifiedRelationshipPropertyPredicates = reifiedRelationshipPropertyPredicates._1,
      reifiedRelationshipSourcePropertyPredicates = reifiedRelationshipSourcePropertyPredicates._1,
      reifiedRelationshipTargetPropertyPredicates = reifiedRelationshipTargetPropertyPredicates._1,
      unreifiedRelationshipPropertyPredicates = unreifiedRelationshipPropertyPredicates._1,
      reifiedRelationshipInversePropertyPredicates = reifiedRelationshipInversePropertyPredicates._1,
      reifiedRelationshipSourceInversePropertyPredicates = reifiedRelationshipSourceInversePropertyPredicates._1,
      reifiedRelationshipTargetInversePropertyPredicates = reifiedRelationshipTargetInversePropertyPredicates._1,
      unreifiedRelationshipInversePropertyPredicates = unreifiedRelationshipInversePropertyPredicates._1,

      entityExistentialRestrictionAxioms = entityExistentialRestrictionAxioms._1,
      entityUniversalRestrictionAxioms = entityUniversalRestrictionAxioms._1,

      entityScalarDataPropertyExistentialRestrictionAxioms = entityScalarDataPropertyExistentialRestrictionAxioms._1,
      entityScalarDataPropertyParticularRestrictionAxioms = entityScalarDataPropertyParticularRestrictionAxioms._1,
      entityScalarDataPropertyUniversalRestrictionAxioms = entityScalarDataPropertyUniversalRestrictionAxioms._1,
      entityStructuredDataPropertyParticularRestrictionAxioms = entityStructuredDataPropertyParticularRestrictionAxioms._1,

      restrictionStructuredDataPropertyTuples = restrictionStructuredDataPropertyTuples._1,
      restrictionScalarDataPropertyValues = restrictionScalarDataPropertyValues._1,

      aspectSpecializationAxioms = aspectSpecializationAxioms._1,
      conceptSpecializationAxioms = conceptSpecializationAxioms._1,
      reifiedRelationshipSpecializationAxioms = reifiedRelationshipSpecializationAxioms._1,

      subDataPropertyOfAxioms = subDataPropertyOfAxioms._1,
      subObjectPropertyOfAxioms = subObjectPropertyOfAxioms._1,

      rootConceptTaxonomyAxioms = rootConceptTaxonomyAxioms._1,
      anonymousConceptUnionAxioms = anonymousConceptUnionAxioms._1,
      specificDisjointConceptAxioms = specificDisjointConceptAxioms._1,

      annotationPropertyValues = annotationPropertyValues._1
    )

    val ob = ts.copy(
      bundles = ts.bundles.filter(_ != b),

      annotationProperties = annotationProperties._2,

      aspects = aspects._2,
      concepts = concepts._2,

      scalars = scalars._2,
      structures = structures._2,

      conceptDesignationTerminologyAxioms = conceptDesignationTerminologyAxioms._2,
      terminologyExtensionAxioms = terminologyExtensionAxioms._2,
      terminologyNestingAxioms = terminologyNestingAxioms._2,
      bundledTerminologyAxioms = bundledTerminologyAxioms._2,

      binaryScalarRestrictions = binaryScalarRestrictions._2,
      iriScalarRestrictions = iriScalarRestrictions._2,
      numericScalarRestrictions = numericScalarRestrictions._2,
      plainLiteralScalarRestrictions = plainLiteralScalarRestrictions._2,
      scalarOneOfRestrictions = scalarOneOfRestrictions._2,
      scalarOneOfLiteralAxioms = scalarOneOfLiteralAxioms._2,
      stringScalarRestrictions = stringScalarRestrictions._2,
      timeScalarRestrictions = timeScalarRestrictions._2,

      entityScalarDataProperties = entityScalarDataProperties._2,
      entityStructuredDataProperties = entityStructuredDataProperties._2,
      scalarDataProperties = scalarDataProperties._2,
      structuredDataProperties = structuredDataProperties._2,

      reifiedRelationships = reifiedRelationships._2,
      unreifiedRelationships = unreifiedRelationships._2,

      chainRules = chainRules._2,
      ruleBodySegments = ruleBodySegments._2 ++ otherBodySegments._2,

      aspectPredicates = aspectPredicates._2,
      conceptPredicates = conceptPredicates._2,
      reifiedRelationshipPredicates = reifiedRelationshipPredicates._2,
      reifiedRelationshipPropertyPredicates = reifiedRelationshipPropertyPredicates._2,
      reifiedRelationshipSourcePropertyPredicates = reifiedRelationshipSourcePropertyPredicates._2,
      reifiedRelationshipTargetPropertyPredicates = reifiedRelationshipTargetPropertyPredicates._2,
      unreifiedRelationshipPropertyPredicates = unreifiedRelationshipPropertyPredicates._2,
      reifiedRelationshipInversePropertyPredicates = reifiedRelationshipInversePropertyPredicates._2,
      reifiedRelationshipSourceInversePropertyPredicates = reifiedRelationshipSourceInversePropertyPredicates._2,
      reifiedRelationshipTargetInversePropertyPredicates = reifiedRelationshipTargetInversePropertyPredicates._2,
      unreifiedRelationshipInversePropertyPredicates = unreifiedRelationshipInversePropertyPredicates._2,

      entityExistentialRestrictionAxioms = entityExistentialRestrictionAxioms._2,
      entityUniversalRestrictionAxioms = entityUniversalRestrictionAxioms._2,

      entityScalarDataPropertyExistentialRestrictionAxioms = entityScalarDataPropertyExistentialRestrictionAxioms._2,
      entityScalarDataPropertyParticularRestrictionAxioms = entityScalarDataPropertyParticularRestrictionAxioms._2,
      entityScalarDataPropertyUniversalRestrictionAxioms = entityScalarDataPropertyUniversalRestrictionAxioms._2,
      entityStructuredDataPropertyParticularRestrictionAxioms = entityStructuredDataPropertyParticularRestrictionAxioms._2,

      restrictionStructuredDataPropertyTuples = restrictionStructuredDataPropertyTuples._2,
      restrictionScalarDataPropertyValues = restrictionScalarDataPropertyValues._2,

      aspectSpecializationAxioms = aspectSpecializationAxioms._2,
      conceptSpecializationAxioms = conceptSpecializationAxioms._2,
      reifiedRelationshipSpecializationAxioms = reifiedRelationshipSpecializationAxioms._2,

      subDataPropertyOfAxioms = subDataPropertyOfAxioms._2,
      subObjectPropertyOfAxioms = subObjectPropertyOfAxioms._2,

      rootConceptTaxonomyAxioms = rootConceptTaxonomyAxioms._2,
      anonymousConceptUnionAxioms = anonymousConceptUnionAxioms._2,
      specificDisjointConceptAxioms = specificDisjointConceptAxioms._2,

      annotationPropertyValues = annotationPropertyValues._2
    )

    (b.iri, tb, ob)
  }

  protected def partitionDescriptionBox
  (d: tables.DescriptionBox, ts: OMLSpecificationTables)
  : (tables.taggedTypes.IRI, OMLSpecificationTables, OMLSpecificationTables)
  = {
    val annotationProperties
    = ts.annotationProperties.partition(_.moduleUUID == d.uuid)

    val descriptionBoxExtendsClosedWorldDefinitions
    = ts.descriptionBoxExtendsClosedWorldDefinitions.partition(_.descriptionBoxUUID == d.uuid)
    val descriptionBoxRefinements
    = ts.descriptionBoxRefinements.partition(_.refiningDescriptionBoxUUID == d.uuid)

    val conceptInstances
    = ts.conceptInstances.partition(_.descriptionBoxUUID == d.uuid)
    val reifiedRelationshipInstances
    = ts.reifiedRelationshipInstances.partition(_.descriptionBoxUUID == d.uuid)
    val reifiedRelationshipInstanceDomains
    = ts.reifiedRelationshipInstanceDomains.partition(_.descriptionBoxUUID == d.uuid)
    val reifiedRelationshipInstanceRanges
    = ts.reifiedRelationshipInstanceRanges.partition(_.descriptionBoxUUID == d.uuid)
    val unreifiedRelationshipInstanceTuples
    = ts.unreifiedRelationshipInstanceTuples.partition(_.descriptionBoxUUID == d.uuid)
    val singletonInstanceStructuredDataPropertyValues
    = ts.singletonInstanceStructuredDataPropertyValues.partition(_.descriptionBoxUUID == d.uuid)
    val singletonInstanceScalarDataPropertyValues
    = ts.singletonInstanceScalarDataPropertyValues.partition(_.descriptionBoxUUID == d.uuid)

    val structuredDataPropertyTuples
    = collectAndPartitionStructuredDataPropertyTuples(
      singletonInstanceStructuredDataPropertyValues._1.map(_.uuid),
      Seq.empty,
      ts.structuredDataPropertyTuples)

    val singletonInstanceStructuredDataPropertyContextUUIDs
    = singletonInstanceStructuredDataPropertyValues._1.map(_.uuid) ++
      structuredDataPropertyTuples._1.map(_.uuid)

    val scalarDataPropertyValues
    = ts.scalarDataPropertyValues.partition { v =>
      singletonInstanceStructuredDataPropertyContextUUIDs.contains(v.structuredDataPropertyContextUUID)
    }

    val allLogicalUUIDs
    : Seq[tables.taggedTypes.LogicalElementUUID]
    = Seq.empty[tables.taggedTypes.LogicalElementUUID] ++
      Seq(d.uuid) ++
      descriptionBoxExtendsClosedWorldDefinitions._1.map(_.uuid) ++
      descriptionBoxRefinements._1.map(_.uuid) ++
      conceptInstances._1.map(_.uuid) ++
      reifiedRelationshipInstances._1.map(_.uuid) ++
      reifiedRelationshipInstanceDomains._1.map(_.uuid) ++
      reifiedRelationshipInstanceRanges._1.map(_.uuid) ++
      unreifiedRelationshipInstanceTuples._1.map(_.uuid) ++
      singletonInstanceStructuredDataPropertyValues._1.map(_.uuid) ++
      singletonInstanceScalarDataPropertyValues._1.map(_.uuid) ++
      structuredDataPropertyTuples._1.map(_.uuid) ++
      scalarDataPropertyValues._1.map(_.uuid)

    val annotationPropertyValues
    = ts.annotationPropertyValues.partition(apv => allLogicalUUIDs.contains(apv.subjectUUID))

    val td = OMLSpecificationTables(
      descriptionBoxes = Seq(d),

      annotationProperties = annotationProperties._1,

      descriptionBoxExtendsClosedWorldDefinitions = descriptionBoxExtendsClosedWorldDefinitions._1,
      descriptionBoxRefinements = descriptionBoxRefinements._1,

      conceptInstances = conceptInstances._1,
      reifiedRelationshipInstances = reifiedRelationshipInstances._1,
      reifiedRelationshipInstanceDomains = reifiedRelationshipInstanceDomains._1,
      reifiedRelationshipInstanceRanges = reifiedRelationshipInstanceRanges._1,
      unreifiedRelationshipInstanceTuples = unreifiedRelationshipInstanceTuples._1,

      singletonInstanceStructuredDataPropertyValues = singletonInstanceStructuredDataPropertyValues._1,
      singletonInstanceScalarDataPropertyValues = singletonInstanceScalarDataPropertyValues._1,
      structuredDataPropertyTuples = structuredDataPropertyTuples._1,
      scalarDataPropertyValues = scalarDataPropertyValues._1,

      annotationPropertyValues = annotationPropertyValues._1
    )

    val od = ts.copy(
      descriptionBoxes = ts.descriptionBoxes.filter(_ != d),

      annotationProperties = annotationProperties._2,

      descriptionBoxExtendsClosedWorldDefinitions = descriptionBoxExtendsClosedWorldDefinitions._2,
      descriptionBoxRefinements = descriptionBoxRefinements._2,

      conceptInstances = conceptInstances._2,
      reifiedRelationshipInstances = reifiedRelationshipInstances._2,
      reifiedRelationshipInstanceDomains = reifiedRelationshipInstanceDomains._2,
      reifiedRelationshipInstanceRanges = reifiedRelationshipInstanceRanges._2,
      unreifiedRelationshipInstanceTuples = unreifiedRelationshipInstanceTuples._2,

      singletonInstanceStructuredDataPropertyValues = singletonInstanceStructuredDataPropertyValues._2,
      singletonInstanceScalarDataPropertyValues = singletonInstanceScalarDataPropertyValues._2,
      structuredDataPropertyTuples = structuredDataPropertyTuples._2,
      scalarDataPropertyValues = scalarDataPropertyValues._2,

      annotationPropertyValues = annotationPropertyValues._2
    )

    (d.iri, td, od)
  }

  /**
    * Partition information in OMLSpecificationTables by OML Module.
    *
    * @param ts The combined OML information in a single OMLSpecificationTables
    * @return A map of OML Module IRI to a corresponding OMLSpecificationTables for its contents.
    */
  def partitionModules
  (ts: OMLSpecificationTables)
  : Map[tables.taggedTypes.IRI, OMLSpecificationTables]
  = {

    @scala.annotation.tailrec
    def extractTerminologyGraphs
    (acc: Map[tables.taggedTypes.IRI, OMLSpecificationTables],
     omlTables: OMLSpecificationTables)
    : (Map[tables.taggedTypes.IRI, OMLSpecificationTables], OMLSpecificationTables)
    = omlTables.terminologyGraphs match {
      case Nil =>
        (acc, omlTables)
      case (g :: _) =>
        val (gIRI, gTables, other) = partitionTerminologyGraph(g, omlTables)
        extractTerminologyGraphs(acc + (gIRI -> gTables), other)
    }

    @scala.annotation.tailrec
    def extractBundles
    (acc: Map[tables.taggedTypes.IRI, OMLSpecificationTables],
     omlTables: OMLSpecificationTables)
    : (Map[tables.taggedTypes.IRI, OMLSpecificationTables], OMLSpecificationTables)
    = omlTables.bundles match {
      case Nil =>
        (acc, omlTables)
      case (b :: _) =>
        val (bIRI, bTables, other) = partitionBundle(b, omlTables)
        extractBundles(acc + (bIRI -> bTables), other)
    }

    @scala.annotation.tailrec
    def extractDescriptionBoxes
    (acc: Map[tables.taggedTypes.IRI, OMLSpecificationTables],
     omlTables: OMLSpecificationTables)
    : (Map[tables.taggedTypes.IRI, OMLSpecificationTables], OMLSpecificationTables)
    = omlTables.descriptionBoxes match {
      case Nil =>
        (acc, omlTables)
      case (d :: _) =>
        val (dIRI, dTables, other) = partitionDescriptionBox(d, omlTables)
        extractDescriptionBoxes(acc + (dIRI -> dTables), other)
    }

    val (graphs, rest1) = extractTerminologyGraphs(Map.empty, ts)
    val (bundles, rest2) = extractBundles(Map.empty, rest1)
    val (descriptionBoxes, rest3) = extractDescriptionBoxes(Map.empty, rest2)

    require(rest3.isEmpty)

    graphs ++ bundles ++ descriptionBoxes
  }

  /**
    * Read a single '*.omlzip' file.
    *
    * @param file
    * @return The OMLSpecificationTables contents read from `file`.
    */
  def readOMLZipFile
  (file: Path)
  : OMLSpecificationTables
  = {
    import scala.collection.JavaConversions.enumerationAsScalaIterator
    val zip = new ZipFile(file.toIO)
    val result =
      zip
        .getEntries
        .to[Seq]
        .par
        .aggregate(OMLSpecificationTables.createEmptyOMLSpecificationTables())(
          seqop = OMLSpecificationTables.readZipArchive(zip),
          combop = OMLSpecificationTables.mergeTables)

    zip.close()

    result
  }

  /**
    * Read `*.omlzip` files and aggregate the OML tables from each file.
    *
    * Note: The aggregation is done in parallel across all `*.omlzip` files
    * and across all OML tables within each file.
    *
    * @param omlZips A set of `*.omlzip` files to read
    * @return The aggregated OML tables read from all `*.omlzip` files.
    */
  def parallelReadOMLZipFiles
  (omlZips: Seq[Path])
  : OMLSpecificationTables
  = {
    def readOMLZipFile
    (prev: OMLSpecificationTables, file: Path)
    : OMLSpecificationTables
    = {
      val zip = new ZipFile(file.toIO)
      val next =
        zip
          .getEntries
          .to[Seq]
          .par
          .aggregate(prev)(
            seqop = OMLSpecificationTables.readZipArchive(zip),
            combop = OMLSpecificationTables.mergeTables)

      zip.close()

      next
    }

    omlZips
      .par
      .aggregate[OMLSpecificationTables](
      OMLSpecificationTables.createEmptyOMLSpecificationTables()
    )(seqop=readOMLZipFile, combop=OMLSpecificationTables.mergeTables)
  }


  /**
    * Read `*.omlzip` files and aggregate the OML tables from each file.
    *
    * Note: The aggregation is done sequentially across all `*.omlzip` files
    * and across all OML tables within each file.
    *
    * @param omlZips A set of `*.omlzip` files to read
    * @return The aggregated OML tables read from all `*.omlzip` files.
    */
  def readOMLZipFiles
  (omlZips: Seq[Path])
  : OMLSpecificationTables
  = {
    def readOMLZipFile
    (prev: OMLSpecificationTables, file: Path)
    : OMLSpecificationTables
    = {
      val zip = new ZipFile(file.toIO)
      val next =
        zip
          .getEntries
          .to[Seq]
          .foldLeft(prev)(OMLSpecificationTables.readZipArchive(zip))

      zip.close()

      next
    }

    val result = omlZips
      .foldLeft(OMLSpecificationTables.createEmptyOMLSpecificationTables()) { case (acc1, tablesPath) =>
        val acc2 = readOMLZipFile(acc1, tablesPath)
        val acc3 = OMLSpecificationTables.mergeTables(acc1, acc2)
        acc3
      }

    result
  }

}
