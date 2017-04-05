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

package gov.nasa.jpl.imce.oml.resolver.impl

import gov.nasa.jpl.imce.oml._

case class OMLResolvedFactoryImpl() extends resolver.api.OMLResolvedFactory {
	
  // Annotation
  
  def createAnnotation
  ( subject: resolver.api.Element,
    property: resolver.api.AnnotationProperty,
    value: scala.Predef.String)
  : resolver.api.Annotation
  = resolver.impl.Annotation(
    subject,
    property,
    value )
  
  // AnnotationEntry
  
  def createAnnotationEntry
  ( module: resolver.api.Module,
    subject: resolver.api.Element,
    value: scala.Predef.String)
  : resolver.api.AnnotationEntry
  = resolver.impl.AnnotationEntry(
    module,
    subject,
    value )
  
  // AnnotationProperty
  
  def createAnnotationProperty
  ( uuid: java.util.UUID,
    iri: gov.nasa.jpl.imce.oml.tables.IRI,
    abbrevIRI: gov.nasa.jpl.imce.oml.tables.AbbrevIRI)
  : resolver.api.AnnotationProperty
  = resolver.impl.AnnotationProperty(
    uuid,
    iri,
    abbrevIRI )
  
  // AnonymousConceptTaxonomyAxiom
  
  def createAnonymousConceptTaxonomyAxiom
  ( uuid: java.util.UUID,
    disjointTaxonomyParent: resolver.api.ConceptTreeDisjunction)
  : resolver.api.AnonymousConceptTaxonomyAxiom
  = resolver.impl.AnonymousConceptTaxonomyAxiom(
    uuid,
    disjointTaxonomyParent )
  
  // Aspect
  
  def createAspect
  ( uuid: java.util.UUID,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.Aspect
  = resolver.impl.Aspect(
    uuid,
    name )
  
  // AspectSpecializationAxiom
  
  def createAspectSpecializationAxiom
  ( uuid: java.util.UUID,
    superAspect: resolver.api.Aspect,
    subEntity: resolver.api.Entity)
  : resolver.api.AspectSpecializationAxiom
  = resolver.impl.AspectSpecializationAxiom(
    uuid,
    superAspect,
    subEntity )
  
  // BinaryScalarRestriction
  
  def createBinaryScalarRestriction
  ( uuid: java.util.UUID,
    restrictedRange: resolver.api.DataRange,
    length: scala.Option[scala.Int],
    minLength: scala.Option[scala.Int],
    maxLength: scala.Option[scala.Int],
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.BinaryScalarRestriction
  = resolver.impl.BinaryScalarRestriction(
    uuid,
    restrictedRange,
    length,
    minLength,
    maxLength,
    name )
  
  // Bundle
  
  def createBundle
  ( uuid: java.util.UUID,
    kind: gov.nasa.jpl.imce.oml.tables.TerminologyKind,
    iri: gov.nasa.jpl.imce.oml.tables.IRI)
  : resolver.api.Bundle
  = resolver.impl.Bundle(
    uuid,
    kind,
    iri )
  
  // BundledTerminologyAxiom
  
  def createBundledTerminologyAxiom
  ( uuid: java.util.UUID,
    bundledTerminology: resolver.api.TerminologyBox)
  : resolver.api.BundledTerminologyAxiom
  = resolver.impl.BundledTerminologyAxiom(
    uuid,
    bundledTerminology )
  
  def copyBundledTerminologyAxiom_bundledTerminology
  ( that: resolver.api.BundledTerminologyAxiom,
    bundledTerminology: resolver.api.TerminologyBox )
  : resolver.api.BundledTerminologyAxiom
  = that match {
  	case x: resolver.impl.BundledTerminologyAxiom =>
  	  x.copy(bundledTerminology = bundledTerminology)
  }
  
  // Concept
  
  def createConcept
  ( uuid: java.util.UUID,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.Concept
  = resolver.impl.Concept(
    uuid,
    name )
  
  // ConceptDesignationTerminologyAxiom
  
  def createConceptDesignationTerminologyAxiom
  ( uuid: java.util.UUID,
    designatedConcept: resolver.api.Concept,
    designatedTerminology: resolver.api.TerminologyBox)
  : resolver.api.ConceptDesignationTerminologyAxiom
  = resolver.impl.ConceptDesignationTerminologyAxiom(
    uuid,
    designatedConcept,
    designatedTerminology )
  
  // ConceptInstance
  
  def createConceptInstance
  ( uuid: java.util.UUID,
    singletonConceptClassifier: resolver.api.Concept,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.ConceptInstance
  = resolver.impl.ConceptInstance(
    uuid,
    singletonConceptClassifier,
    name )
  
  // ConceptSpecializationAxiom
  
  def createConceptSpecializationAxiom
  ( uuid: java.util.UUID,
    superConcept: resolver.api.Concept,
    subConcept: resolver.api.Concept)
  : resolver.api.ConceptSpecializationAxiom
  = resolver.impl.ConceptSpecializationAxiom(
    uuid,
    superConcept,
    subConcept )
  
  // DataStructureTuple
  
  def createDataStructureTuple
  ( uuid: java.util.UUID,
    dataStructureType: resolver.api.Structure,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.DataStructureTuple
  = resolver.impl.DataStructureTuple(
    uuid,
    dataStructureType,
    name )
  
  // DescriptionBox
  
  def createDescriptionBox
  ( uuid: java.util.UUID,
    kind: gov.nasa.jpl.imce.oml.tables.DescriptionKind,
    iri: gov.nasa.jpl.imce.oml.tables.IRI)
  : resolver.api.DescriptionBox
  = resolver.impl.DescriptionBox(
    uuid,
    kind,
    iri )
  
  // DescriptionBoxExtendsClosedWorldDefinitions
  
  def createDescriptionBoxExtendsClosedWorldDefinitions
  ( uuid: java.util.UUID,
    closedWorldDefinitions: resolver.api.TerminologyBox)
  : resolver.api.DescriptionBoxExtendsClosedWorldDefinitions
  = resolver.impl.DescriptionBoxExtendsClosedWorldDefinitions(
    uuid,
    closedWorldDefinitions )
  
  // DescriptionBoxRefinement
  
  def createDescriptionBoxRefinement
  ( uuid: java.util.UUID,
    refinedDescriptionBox: resolver.api.DescriptionBox)
  : resolver.api.DescriptionBoxRefinement
  = resolver.impl.DescriptionBoxRefinement(
    uuid,
    refinedDescriptionBox )
  
  // EntityExistentialRestrictionAxiom
  
  def createEntityExistentialRestrictionAxiom
  ( uuid: java.util.UUID,
    restrictedRelation: resolver.api.EntityRelationship,
    restrictedDomain: resolver.api.Entity,
    restrictedRange: resolver.api.Entity)
  : resolver.api.EntityExistentialRestrictionAxiom
  = resolver.impl.EntityExistentialRestrictionAxiom(
    uuid,
    restrictedRelation,
    restrictedDomain,
    restrictedRange )
  
  // EntityScalarDataProperty
  
  def createEntityScalarDataProperty
  ( uuid: java.util.UUID,
    domain: resolver.api.Entity,
    range: resolver.api.DataRange,
    isIdentityCriteria: scala.Boolean,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.EntityScalarDataProperty
  = resolver.impl.EntityScalarDataProperty(
    uuid,
    domain,
    range,
    isIdentityCriteria,
    name )
  
  // EntityScalarDataPropertyExistentialRestrictionAxiom
  
  def createEntityScalarDataPropertyExistentialRestrictionAxiom
  ( uuid: java.util.UUID,
    restrictedEntity: resolver.api.Entity,
    scalarProperty: resolver.api.EntityScalarDataProperty,
    scalarRestriction: resolver.api.DataRange)
  : resolver.api.EntityScalarDataPropertyExistentialRestrictionAxiom
  = resolver.impl.EntityScalarDataPropertyExistentialRestrictionAxiom(
    uuid,
    restrictedEntity,
    scalarProperty,
    scalarRestriction )
  
  // EntityScalarDataPropertyParticularRestrictionAxiom
  
  def createEntityScalarDataPropertyParticularRestrictionAxiom
  ( uuid: java.util.UUID,
    restrictedEntity: resolver.api.Entity,
    scalarProperty: resolver.api.EntityScalarDataProperty,
    literalValue: gov.nasa.jpl.imce.oml.tables.LexicalValue)
  : resolver.api.EntityScalarDataPropertyParticularRestrictionAxiom
  = resolver.impl.EntityScalarDataPropertyParticularRestrictionAxiom(
    uuid,
    restrictedEntity,
    scalarProperty,
    literalValue )
  
  // EntityScalarDataPropertyUniversalRestrictionAxiom
  
  def createEntityScalarDataPropertyUniversalRestrictionAxiom
  ( uuid: java.util.UUID,
    restrictedEntity: resolver.api.Entity,
    scalarProperty: resolver.api.EntityScalarDataProperty,
    scalarRestriction: resolver.api.DataRange)
  : resolver.api.EntityScalarDataPropertyUniversalRestrictionAxiom
  = resolver.impl.EntityScalarDataPropertyUniversalRestrictionAxiom(
    uuid,
    restrictedEntity,
    scalarProperty,
    scalarRestriction )
  
  // EntityStructuredDataProperty
  
  def createEntityStructuredDataProperty
  ( uuid: java.util.UUID,
    domain: resolver.api.Entity,
    range: resolver.api.Structure,
    isIdentityCriteria: scala.Boolean,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.EntityStructuredDataProperty
  = resolver.impl.EntityStructuredDataProperty(
    uuid,
    domain,
    range,
    isIdentityCriteria,
    name )
  
  // EntityUniversalRestrictionAxiom
  
  def createEntityUniversalRestrictionAxiom
  ( uuid: java.util.UUID,
    restrictedRelation: resolver.api.EntityRelationship,
    restrictedDomain: resolver.api.Entity,
    restrictedRange: resolver.api.Entity)
  : resolver.api.EntityUniversalRestrictionAxiom
  = resolver.impl.EntityUniversalRestrictionAxiom(
    uuid,
    restrictedRelation,
    restrictedDomain,
    restrictedRange )
  
  // Extent
  
  def createExtent
  : resolver.api.Extent
  = resolver.impl.Extent
  
  // IRIScalarRestriction
  
  def createIRIScalarRestriction
  ( uuid: java.util.UUID,
    restrictedRange: resolver.api.DataRange,
    length: scala.Option[scala.Int],
    minLength: scala.Option[scala.Int],
    maxLength: scala.Option[scala.Int],
    name: gov.nasa.jpl.imce.oml.tables.LocalName,
    pattern: scala.Option[gov.nasa.jpl.imce.oml.tables.Pattern])
  : resolver.api.IRIScalarRestriction
  = resolver.impl.IRIScalarRestriction(
    uuid,
    restrictedRange,
    length,
    minLength,
    maxLength,
    name,
    pattern )
  
  // NumericScalarRestriction
  
  def createNumericScalarRestriction
  ( uuid: java.util.UUID,
    restrictedRange: resolver.api.DataRange,
    minExclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalNumber],
    minInclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalNumber],
    maxExclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalNumber],
    maxInclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalNumber],
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.NumericScalarRestriction
  = resolver.impl.NumericScalarRestriction(
    uuid,
    restrictedRange,
    minExclusive,
    minInclusive,
    maxExclusive,
    maxInclusive,
    name )
  
  // PlainLiteralScalarRestriction
  
  def createPlainLiteralScalarRestriction
  ( uuid: java.util.UUID,
    restrictedRange: resolver.api.DataRange,
    length: scala.Option[scala.Int],
    minLength: scala.Option[scala.Int],
    maxLength: scala.Option[scala.Int],
    name: gov.nasa.jpl.imce.oml.tables.LocalName,
    langRange: scala.Option[gov.nasa.jpl.imce.oml.tables.LangRange],
    pattern: scala.Option[gov.nasa.jpl.imce.oml.tables.Pattern])
  : resolver.api.PlainLiteralScalarRestriction
  = resolver.impl.PlainLiteralScalarRestriction(
    uuid,
    restrictedRange,
    length,
    minLength,
    maxLength,
    name,
    langRange,
    pattern )
  
  // ReifiedRelationship
  
  def createReifiedRelationship
  ( uuid: java.util.UUID,
    source: resolver.api.Entity,
    target: resolver.api.Entity,
    isAsymmetric: scala.Boolean,
    isEssential: scala.Boolean,
    isFunctional: scala.Boolean,
    isInverseEssential: scala.Boolean,
    isInverseFunctional: scala.Boolean,
    isIrreflexive: scala.Boolean,
    isReflexive: scala.Boolean,
    isSymmetric: scala.Boolean,
    isTransitive: scala.Boolean,
    name: gov.nasa.jpl.imce.oml.tables.LocalName,
    unreifiedPropertyName: gov.nasa.jpl.imce.oml.tables.LocalName,
    unreifiedInversePropertyName: scala.Option[gov.nasa.jpl.imce.oml.tables.LocalName])
  : resolver.api.ReifiedRelationship
  = resolver.impl.ReifiedRelationship(
    uuid,
    source,
    target,
    isAsymmetric,
    isEssential,
    isFunctional,
    isInverseEssential,
    isInverseFunctional,
    isIrreflexive,
    isReflexive,
    isSymmetric,
    isTransitive,
    name,
    unreifiedPropertyName,
    unreifiedInversePropertyName )
  
  // ReifiedRelationshipInstance
  
  def createReifiedRelationshipInstance
  ( uuid: java.util.UUID,
    singletonReifiedRelationshipClassifier: resolver.api.ReifiedRelationship,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.ReifiedRelationshipInstance
  = resolver.impl.ReifiedRelationshipInstance(
    uuid,
    singletonReifiedRelationshipClassifier,
    name )
  
  // ReifiedRelationshipInstanceDomain
  
  def createReifiedRelationshipInstanceDomain
  ( uuid: java.util.UUID,
    reifiedRelationshipInstance: resolver.api.ReifiedRelationshipInstance,
    domain: resolver.api.ConceptualEntitySingletonInstance,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.ReifiedRelationshipInstanceDomain
  = resolver.impl.ReifiedRelationshipInstanceDomain(
    uuid,
    reifiedRelationshipInstance,
    domain,
    name )
  
  // ReifiedRelationshipInstanceRange
  
  def createReifiedRelationshipInstanceRange
  ( uuid: java.util.UUID,
    reifiedRelationshipInstance: resolver.api.ReifiedRelationshipInstance,
    range: resolver.api.ConceptualEntitySingletonInstance,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.ReifiedRelationshipInstanceRange
  = resolver.impl.ReifiedRelationshipInstanceRange(
    uuid,
    reifiedRelationshipInstance,
    range,
    name )
  
  // ReifiedRelationshipSpecializationAxiom
  
  def createReifiedRelationshipSpecializationAxiom
  ( uuid: java.util.UUID,
    superRelationship: resolver.api.ReifiedRelationship,
    subRelationship: resolver.api.ReifiedRelationship)
  : resolver.api.ReifiedRelationshipSpecializationAxiom
  = resolver.impl.ReifiedRelationshipSpecializationAxiom(
    uuid,
    superRelationship,
    subRelationship )
  
  // RootConceptTaxonomyAxiom
  
  def createRootConceptTaxonomyAxiom
  ( uuid: java.util.UUID,
    root: resolver.api.Concept)
  : resolver.api.RootConceptTaxonomyAxiom
  = resolver.impl.RootConceptTaxonomyAxiom(
    uuid,
    root )
  
  // Scalar
  
  def createScalar
  ( uuid: java.util.UUID,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.Scalar
  = resolver.impl.Scalar(
    uuid,
    name )
  
  // ScalarDataProperty
  
  def createScalarDataProperty
  ( uuid: java.util.UUID,
    domain: resolver.api.Structure,
    range: resolver.api.DataRange,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.ScalarDataProperty
  = resolver.impl.ScalarDataProperty(
    uuid,
    domain,
    range,
    name )
  
  // ScalarDataPropertyValue
  
  def createScalarDataPropertyValue
  ( uuid: java.util.UUID,
    scalarDataProperty: resolver.api.DataRelationshipToScalar,
    name: gov.nasa.jpl.imce.oml.tables.LocalName,
    scalarPropertyValue: scala.Predef.String)
  : resolver.api.ScalarDataPropertyValue
  = resolver.impl.ScalarDataPropertyValue(
    uuid,
    scalarDataProperty,
    name,
    scalarPropertyValue )
  
  // ScalarOneOfLiteralAxiom
  
  def createScalarOneOfLiteralAxiom
  ( uuid: java.util.UUID,
    axiom: resolver.api.ScalarOneOfRestriction,
    value: gov.nasa.jpl.imce.oml.tables.LexicalValue)
  : resolver.api.ScalarOneOfLiteralAxiom
  = resolver.impl.ScalarOneOfLiteralAxiom(
    uuid,
    axiom,
    value )
  
  // ScalarOneOfRestriction
  
  def createScalarOneOfRestriction
  ( uuid: java.util.UUID,
    restrictedRange: resolver.api.DataRange,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.ScalarOneOfRestriction
  = resolver.impl.ScalarOneOfRestriction(
    uuid,
    restrictedRange,
    name )
  
  // SpecificDisjointConceptAxiom
  
  def createSpecificDisjointConceptAxiom
  ( uuid: java.util.UUID,
    disjointTaxonomyParent: resolver.api.ConceptTreeDisjunction,
    disjointLeaf: resolver.api.Concept)
  : resolver.api.SpecificDisjointConceptAxiom
  = resolver.impl.SpecificDisjointConceptAxiom(
    uuid,
    disjointTaxonomyParent,
    disjointLeaf )
  
  // StringScalarRestriction
  
  def createStringScalarRestriction
  ( uuid: java.util.UUID,
    restrictedRange: resolver.api.DataRange,
    length: scala.Option[scala.Int],
    minLength: scala.Option[scala.Int],
    maxLength: scala.Option[scala.Int],
    name: gov.nasa.jpl.imce.oml.tables.LocalName,
    pattern: scala.Option[gov.nasa.jpl.imce.oml.tables.Pattern])
  : resolver.api.StringScalarRestriction
  = resolver.impl.StringScalarRestriction(
    uuid,
    restrictedRange,
    length,
    minLength,
    maxLength,
    name,
    pattern )
  
  // Structure
  
  def createStructure
  ( uuid: java.util.UUID,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.Structure
  = resolver.impl.Structure(
    uuid,
    name )
  
  // StructuredDataProperty
  
  def createStructuredDataProperty
  ( uuid: java.util.UUID,
    domain: resolver.api.Structure,
    range: resolver.api.Structure,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.StructuredDataProperty
  = resolver.impl.StructuredDataProperty(
    uuid,
    domain,
    range,
    name )
  
  // StructuredDataPropertyValue
  
  def createStructuredDataPropertyValue
  ( uuid: java.util.UUID,
    structuredDataProperty: resolver.api.DataRelationshipToStructure,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.StructuredDataPropertyValue
  = resolver.impl.StructuredDataPropertyValue(
    uuid,
    structuredDataProperty,
    name )
  
  // SynonymScalarRestriction
  
  def createSynonymScalarRestriction
  ( uuid: java.util.UUID,
    restrictedRange: resolver.api.DataRange,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.SynonymScalarRestriction
  = resolver.impl.SynonymScalarRestriction(
    uuid,
    restrictedRange,
    name )
  
  // TerminologyExtensionAxiom
  
  def createTerminologyExtensionAxiom
  ( uuid: java.util.UUID,
    extendedTerminology: resolver.api.TerminologyBox)
  : resolver.api.TerminologyExtensionAxiom
  = resolver.impl.TerminologyExtensionAxiom(
    uuid,
    extendedTerminology )
  
  def copyTerminologyExtensionAxiom_extendedTerminology
  ( that: resolver.api.TerminologyExtensionAxiom,
    extendedTerminology: resolver.api.TerminologyBox )
  : resolver.api.TerminologyExtensionAxiom
  = that match {
  	case x: resolver.impl.TerminologyExtensionAxiom =>
  	  x.copy(extendedTerminology = extendedTerminology)
  }
  
  // TerminologyGraph
  
  def createTerminologyGraph
  ( uuid: java.util.UUID,
    kind: gov.nasa.jpl.imce.oml.tables.TerminologyKind,
    iri: gov.nasa.jpl.imce.oml.tables.IRI)
  : resolver.api.TerminologyGraph
  = resolver.impl.TerminologyGraph(
    uuid,
    kind,
    iri )
  
  // TerminologyNestingAxiom
  
  def createTerminologyNestingAxiom
  ( uuid: java.util.UUID,
    nestingTerminology: resolver.api.TerminologyBox,
    nestingContext: resolver.api.Concept)
  : resolver.api.TerminologyNestingAxiom
  = resolver.impl.TerminologyNestingAxiom(
    uuid,
    nestingTerminology,
    nestingContext )
  
  def copyTerminologyNestingAxiom_nestingTerminology
  ( that: resolver.api.TerminologyNestingAxiom,
    nestingTerminology: resolver.api.TerminologyBox )
  : resolver.api.TerminologyNestingAxiom
  = that match {
  	case x: resolver.impl.TerminologyNestingAxiom =>
  	  x.copy(nestingTerminology = nestingTerminology)
  }
  
  // TimeScalarRestriction
  
  def createTimeScalarRestriction
  ( uuid: java.util.UUID,
    restrictedRange: resolver.api.DataRange,
    minExclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalTime],
    minInclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalTime],
    maxExclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalTime],
    maxInclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalTime],
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.TimeScalarRestriction
  = resolver.impl.TimeScalarRestriction(
    uuid,
    restrictedRange,
    minExclusive,
    minInclusive,
    maxExclusive,
    maxInclusive,
    name )
  
  // UnreifiedRelationship
  
  def createUnreifiedRelationship
  ( uuid: java.util.UUID,
    source: resolver.api.Entity,
    target: resolver.api.Entity,
    isAsymmetric: scala.Boolean,
    isEssential: scala.Boolean,
    isFunctional: scala.Boolean,
    isInverseEssential: scala.Boolean,
    isInverseFunctional: scala.Boolean,
    isIrreflexive: scala.Boolean,
    isReflexive: scala.Boolean,
    isSymmetric: scala.Boolean,
    isTransitive: scala.Boolean,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.UnreifiedRelationship
  = resolver.impl.UnreifiedRelationship(
    uuid,
    source,
    target,
    isAsymmetric,
    isEssential,
    isFunctional,
    isInverseEssential,
    isInverseFunctional,
    isIrreflexive,
    isReflexive,
    isSymmetric,
    isTransitive,
    name )
  
  // UnreifiedRelationshipInstanceTuple
  
  def createUnreifiedRelationshipInstanceTuple
  ( uuid: java.util.UUID,
    unreifiedRelationship: resolver.api.UnreifiedRelationship,
    domain: resolver.api.ConceptualEntitySingletonInstance,
    range: resolver.api.ConceptualEntitySingletonInstance,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.UnreifiedRelationshipInstanceTuple
  = resolver.impl.UnreifiedRelationshipInstanceTuple(
    uuid,
    unreifiedRelationship,
    domain,
    range,
    name )
  
}
