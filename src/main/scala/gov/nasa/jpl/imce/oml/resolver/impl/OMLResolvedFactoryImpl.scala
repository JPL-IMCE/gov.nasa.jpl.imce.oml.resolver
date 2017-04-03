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
  ( iri: gov.nasa.jpl.imce.oml.tables.IRI,
    abbrevIRI: gov.nasa.jpl.imce.oml.tables.AbbrevIRI)
  : resolver.api.AnnotationProperty
  = resolver.impl.AnnotationProperty(
    iri,
    abbrevIRI )
  
  // AnonymousConceptTaxonomyAxiom
  
  def createAnonymousConceptTaxonomyAxiom
  ( disjointTaxonomyParent: resolver.api.ConceptTreeDisjunction)
  : resolver.api.AnonymousConceptTaxonomyAxiom
  = resolver.impl.AnonymousConceptTaxonomyAxiom(
    disjointTaxonomyParent )
  
  // Aspect
  
  def createAspect
  ( name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.Aspect
  = resolver.impl.Aspect(
    name )
  
  // AspectSpecializationAxiom
  
  def createAspectSpecializationAxiom
  ( superAspect: resolver.api.Aspect,
    subEntity: resolver.api.Entity)
  : resolver.api.AspectSpecializationAxiom
  = resolver.impl.AspectSpecializationAxiom(
    superAspect,
    subEntity )
  
  // BinaryScalarRestriction
  
  def createBinaryScalarRestriction
  ( restrictedRange: resolver.api.DataRange,
    length: scala.Option[scala.Int],
    minLength: scala.Option[scala.Int],
    maxLength: scala.Option[scala.Int],
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.BinaryScalarRestriction
  = resolver.impl.BinaryScalarRestriction(
    restrictedRange,
    length,
    minLength,
    maxLength,
    name )
  
  // Bundle
  
  def createBundle
  ( kind: gov.nasa.jpl.imce.oml.tables.TerminologyKind,
    iri: gov.nasa.jpl.imce.oml.tables.IRI)
  : resolver.api.Bundle
  = resolver.impl.Bundle(
    kind,
    iri )
  
  // BundledTerminologyAxiom
  
  def createBundledTerminologyAxiom
  ( bundledTerminology: resolver.api.TerminologyBox)
  : resolver.api.BundledTerminologyAxiom
  = resolver.impl.BundledTerminologyAxiom(
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
  ( name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.Concept
  = resolver.impl.Concept(
    name )
  
  // ConceptDesignationTerminologyAxiom
  
  def createConceptDesignationTerminologyAxiom
  ( designatedConcept: resolver.api.Concept,
    designatedTerminology: resolver.api.TerminologyBox)
  : resolver.api.ConceptDesignationTerminologyAxiom
  = resolver.impl.ConceptDesignationTerminologyAxiom(
    designatedConcept,
    designatedTerminology )
  
  // ConceptInstance
  
  def createConceptInstance
  ( singletonConceptClassifier: resolver.api.Concept,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.ConceptInstance
  = resolver.impl.ConceptInstance(
    singletonConceptClassifier,
    name )
  
  // ConceptSpecializationAxiom
  
  def createConceptSpecializationAxiom
  ( superConcept: resolver.api.Concept,
    subConcept: resolver.api.Concept)
  : resolver.api.ConceptSpecializationAxiom
  = resolver.impl.ConceptSpecializationAxiom(
    superConcept,
    subConcept )
  
  // DataStructureTuple
  
  def createDataStructureTuple
  ( dataStructureType: resolver.api.Structure,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.DataStructureTuple
  = resolver.impl.DataStructureTuple(
    dataStructureType,
    name )
  
  // DescriptionBox
  
  def createDescriptionBox
  ( kind: gov.nasa.jpl.imce.oml.tables.DescriptionKind,
    iri: gov.nasa.jpl.imce.oml.tables.IRI)
  : resolver.api.DescriptionBox
  = resolver.impl.DescriptionBox(
    kind,
    iri )
  
  // DescriptionBoxExtendsClosedWorldDefinitions
  
  def createDescriptionBoxExtendsClosedWorldDefinitions
  ( closedWorldDefinitions: resolver.api.TerminologyBox)
  : resolver.api.DescriptionBoxExtendsClosedWorldDefinitions
  = resolver.impl.DescriptionBoxExtendsClosedWorldDefinitions(
    closedWorldDefinitions )
  
  // DescriptionBoxRefinement
  
  def createDescriptionBoxRefinement
  ( refinedDescriptionBox: resolver.api.DescriptionBox)
  : resolver.api.DescriptionBoxRefinement
  = resolver.impl.DescriptionBoxRefinement(
    refinedDescriptionBox )
  
  // EntityExistentialRestrictionAxiom
  
  def createEntityExistentialRestrictionAxiom
  ( restrictedRelation: resolver.api.EntityRelationship,
    restrictedDomain: resolver.api.Entity,
    restrictedRange: resolver.api.Entity)
  : resolver.api.EntityExistentialRestrictionAxiom
  = resolver.impl.EntityExistentialRestrictionAxiom(
    restrictedRelation,
    restrictedDomain,
    restrictedRange )
  
  // EntityScalarDataProperty
  
  def createEntityScalarDataProperty
  ( domain: resolver.api.Entity,
    range: resolver.api.DataRange,
    isIdentityCriteria: scala.Boolean,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.EntityScalarDataProperty
  = resolver.impl.EntityScalarDataProperty(
    domain,
    range,
    isIdentityCriteria,
    name )
  
  // EntityScalarDataPropertyExistentialRestrictionAxiom
  
  def createEntityScalarDataPropertyExistentialRestrictionAxiom
  ( restrictedEntity: resolver.api.Entity,
    scalarProperty: resolver.api.EntityScalarDataProperty,
    scalarRestriction: resolver.api.DataRange)
  : resolver.api.EntityScalarDataPropertyExistentialRestrictionAxiom
  = resolver.impl.EntityScalarDataPropertyExistentialRestrictionAxiom(
    restrictedEntity,
    scalarProperty,
    scalarRestriction )
  
  // EntityScalarDataPropertyParticularRestrictionAxiom
  
  def createEntityScalarDataPropertyParticularRestrictionAxiom
  ( restrictedEntity: resolver.api.Entity,
    scalarProperty: resolver.api.EntityScalarDataProperty,
    literalValue: gov.nasa.jpl.imce.oml.tables.LexicalValue)
  : resolver.api.EntityScalarDataPropertyParticularRestrictionAxiom
  = resolver.impl.EntityScalarDataPropertyParticularRestrictionAxiom(
    restrictedEntity,
    scalarProperty,
    literalValue )
  
  // EntityScalarDataPropertyUniversalRestrictionAxiom
  
  def createEntityScalarDataPropertyUniversalRestrictionAxiom
  ( restrictedEntity: resolver.api.Entity,
    scalarProperty: resolver.api.EntityScalarDataProperty,
    scalarRestriction: resolver.api.DataRange)
  : resolver.api.EntityScalarDataPropertyUniversalRestrictionAxiom
  = resolver.impl.EntityScalarDataPropertyUniversalRestrictionAxiom(
    restrictedEntity,
    scalarProperty,
    scalarRestriction )
  
  // EntityStructuredDataProperty
  
  def createEntityStructuredDataProperty
  ( domain: resolver.api.Entity,
    range: resolver.api.Structure,
    isIdentityCriteria: scala.Boolean,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.EntityStructuredDataProperty
  = resolver.impl.EntityStructuredDataProperty(
    domain,
    range,
    isIdentityCriteria,
    name )
  
  // EntityUniversalRestrictionAxiom
  
  def createEntityUniversalRestrictionAxiom
  ( restrictedRelation: resolver.api.EntityRelationship,
    restrictedDomain: resolver.api.Entity,
    restrictedRange: resolver.api.Entity)
  : resolver.api.EntityUniversalRestrictionAxiom
  = resolver.impl.EntityUniversalRestrictionAxiom(
    restrictedRelation,
    restrictedDomain,
    restrictedRange )
  
  // Extent
  
  def createExtent
  : resolver.api.Extent
  = resolver.impl.Extent
  
  // IRIScalarRestriction
  
  def createIRIScalarRestriction
  ( restrictedRange: resolver.api.DataRange,
    length: scala.Option[scala.Int],
    minLength: scala.Option[scala.Int],
    maxLength: scala.Option[scala.Int],
    name: gov.nasa.jpl.imce.oml.tables.LocalName,
    pattern: scala.Option[gov.nasa.jpl.imce.oml.tables.Pattern])
  : resolver.api.IRIScalarRestriction
  = resolver.impl.IRIScalarRestriction(
    restrictedRange,
    length,
    minLength,
    maxLength,
    name,
    pattern )
  
  // NumericScalarRestriction
  
  def createNumericScalarRestriction
  ( restrictedRange: resolver.api.DataRange,
    minExclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalNumber],
    minInclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalNumber],
    maxExclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalNumber],
    maxInclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalNumber],
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.NumericScalarRestriction
  = resolver.impl.NumericScalarRestriction(
    restrictedRange,
    minExclusive,
    minInclusive,
    maxExclusive,
    maxInclusive,
    name )
  
  // PlainLiteralScalarRestriction
  
  def createPlainLiteralScalarRestriction
  ( restrictedRange: resolver.api.DataRange,
    length: scala.Option[scala.Int],
    minLength: scala.Option[scala.Int],
    maxLength: scala.Option[scala.Int],
    name: gov.nasa.jpl.imce.oml.tables.LocalName,
    langRange: scala.Option[gov.nasa.jpl.imce.oml.tables.LangRange],
    pattern: scala.Option[gov.nasa.jpl.imce.oml.tables.Pattern])
  : resolver.api.PlainLiteralScalarRestriction
  = resolver.impl.PlainLiteralScalarRestriction(
    restrictedRange,
    length,
    minLength,
    maxLength,
    name,
    langRange,
    pattern )
  
  // ReifiedRelationship
  
  def createReifiedRelationship
  ( source: resolver.api.Entity,
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
  ( singletonReifiedRelationshipClassifier: resolver.api.ReifiedRelationship,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.ReifiedRelationshipInstance
  = resolver.impl.ReifiedRelationshipInstance(
    singletonReifiedRelationshipClassifier,
    name )
  
  // ReifiedRelationshipInstanceDomain
  
  def createReifiedRelationshipInstanceDomain
  ( reifiedRelationshipInstance: resolver.api.ReifiedRelationshipInstance,
    domain: resolver.api.ConceptualEntitySingletonInstance,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.ReifiedRelationshipInstanceDomain
  = resolver.impl.ReifiedRelationshipInstanceDomain(
    reifiedRelationshipInstance,
    domain,
    name )
  
  // ReifiedRelationshipInstanceRange
  
  def createReifiedRelationshipInstanceRange
  ( reifiedRelationshipInstance: resolver.api.ReifiedRelationshipInstance,
    range: resolver.api.ConceptualEntitySingletonInstance,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.ReifiedRelationshipInstanceRange
  = resolver.impl.ReifiedRelationshipInstanceRange(
    reifiedRelationshipInstance,
    range,
    name )
  
  // ReifiedRelationshipSpecializationAxiom
  
  def createReifiedRelationshipSpecializationAxiom
  ( superRelationship: resolver.api.ReifiedRelationship,
    subRelationship: resolver.api.ReifiedRelationship)
  : resolver.api.ReifiedRelationshipSpecializationAxiom
  = resolver.impl.ReifiedRelationshipSpecializationAxiom(
    superRelationship,
    subRelationship )
  
  // RootConceptTaxonomyAxiom
  
  def createRootConceptTaxonomyAxiom
  ( root: resolver.api.Concept)
  : resolver.api.RootConceptTaxonomyAxiom
  = resolver.impl.RootConceptTaxonomyAxiom(
    root )
  
  // Scalar
  
  def createScalar
  ( name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.Scalar
  = resolver.impl.Scalar(
    name )
  
  // ScalarDataProperty
  
  def createScalarDataProperty
  ( domain: resolver.api.Structure,
    range: resolver.api.DataRange,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.ScalarDataProperty
  = resolver.impl.ScalarDataProperty(
    domain,
    range,
    name )
  
  // ScalarDataPropertyValue
  
  def createScalarDataPropertyValue
  ( scalarDataProperty: resolver.api.DataRelationshipToScalar,
    name: gov.nasa.jpl.imce.oml.tables.LocalName,
    scalarPropertyValue: scala.Predef.String)
  : resolver.api.ScalarDataPropertyValue
  = resolver.impl.ScalarDataPropertyValue(
    scalarDataProperty,
    name,
    scalarPropertyValue )
  
  // ScalarOneOfLiteralAxiom
  
  def createScalarOneOfLiteralAxiom
  ( axiom: resolver.api.ScalarOneOfRestriction,
    value: gov.nasa.jpl.imce.oml.tables.LexicalValue)
  : resolver.api.ScalarOneOfLiteralAxiom
  = resolver.impl.ScalarOneOfLiteralAxiom(
    axiom,
    value )
  
  // ScalarOneOfRestriction
  
  def createScalarOneOfRestriction
  ( restrictedRange: resolver.api.DataRange,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.ScalarOneOfRestriction
  = resolver.impl.ScalarOneOfRestriction(
    restrictedRange,
    name )
  
  // SpecificDisjointConceptAxiom
  
  def createSpecificDisjointConceptAxiom
  ( disjointTaxonomyParent: resolver.api.ConceptTreeDisjunction,
    disjointLeaf: resolver.api.Concept)
  : resolver.api.SpecificDisjointConceptAxiom
  = resolver.impl.SpecificDisjointConceptAxiom(
    disjointTaxonomyParent,
    disjointLeaf )
  
  // StringScalarRestriction
  
  def createStringScalarRestriction
  ( restrictedRange: resolver.api.DataRange,
    length: scala.Option[scala.Int],
    minLength: scala.Option[scala.Int],
    maxLength: scala.Option[scala.Int],
    name: gov.nasa.jpl.imce.oml.tables.LocalName,
    pattern: scala.Option[gov.nasa.jpl.imce.oml.tables.Pattern])
  : resolver.api.StringScalarRestriction
  = resolver.impl.StringScalarRestriction(
    restrictedRange,
    length,
    minLength,
    maxLength,
    name,
    pattern )
  
  // Structure
  
  def createStructure
  ( name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.Structure
  = resolver.impl.Structure(
    name )
  
  // StructuredDataProperty
  
  def createStructuredDataProperty
  ( domain: resolver.api.Structure,
    range: resolver.api.Structure,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.StructuredDataProperty
  = resolver.impl.StructuredDataProperty(
    domain,
    range,
    name )
  
  // StructuredDataPropertyValue
  
  def createStructuredDataPropertyValue
  ( structuredDataProperty: resolver.api.DataRelationshipToStructure,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.StructuredDataPropertyValue
  = resolver.impl.StructuredDataPropertyValue(
    structuredDataProperty,
    name )
  
  // SynonymScalarRestriction
  
  def createSynonymScalarRestriction
  ( restrictedRange: resolver.api.DataRange,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.SynonymScalarRestriction
  = resolver.impl.SynonymScalarRestriction(
    restrictedRange,
    name )
  
  // TerminologyExtensionAxiom
  
  def createTerminologyExtensionAxiom
  ( extendedTerminology: resolver.api.TerminologyBox)
  : resolver.api.TerminologyExtensionAxiom
  = resolver.impl.TerminologyExtensionAxiom(
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
  ( kind: gov.nasa.jpl.imce.oml.tables.TerminologyKind,
    iri: gov.nasa.jpl.imce.oml.tables.IRI)
  : resolver.api.TerminologyGraph
  = resolver.impl.TerminologyGraph(
    kind,
    iri )
  
  // TerminologyNestingAxiom
  
  def createTerminologyNestingAxiom
  ( nestingTerminology: resolver.api.TerminologyBox,
    nestingContext: resolver.api.Concept)
  : resolver.api.TerminologyNestingAxiom
  = resolver.impl.TerminologyNestingAxiom(
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
  ( restrictedRange: resolver.api.DataRange,
    minExclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalTime],
    minInclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalTime],
    maxExclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalTime],
    maxInclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalTime],
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.TimeScalarRestriction
  = resolver.impl.TimeScalarRestriction(
    restrictedRange,
    minExclusive,
    minInclusive,
    maxExclusive,
    maxInclusive,
    name )
  
  // UnreifiedRelationship
  
  def createUnreifiedRelationship
  ( source: resolver.api.Entity,
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
  ( unreifiedRelationship: resolver.api.UnreifiedRelationship,
    domain: resolver.api.ConceptualEntitySingletonInstance,
    range: resolver.api.ConceptualEntitySingletonInstance,
    name: gov.nasa.jpl.imce.oml.tables.LocalName)
  : resolver.api.UnreifiedRelationshipInstanceTuple
  = resolver.impl.UnreifiedRelationshipInstanceTuple(
    unreifiedRelationship,
    domain,
    range,
    name )
  
}
