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

package gov.nasa.jpl.imce.oml.specification.resolver.impl

import gov.nasa.jpl.imce.oml.specification._

case class OMLResolvedFactoryImpl() extends resolver.api.OMLResolvedFactory {
	
  // Annotation
  
  def createAnnotation
  ( terminology: resolver.api.TerminologyBox,
    subject: resolver.api.TerminologyThing,
    property: resolver.api.AnnotationProperty,
    value: scala.Predef.String)
  : resolver.api.Annotation
  = resolver.impl.Annotation(
    terminology,
    subject,
    property,
    value )
  
  // AnnotationEntry
  
  def createAnnotationEntry
  ( terminology: resolver.api.TerminologyBox,
    subject: resolver.api.TerminologyThing,
    value: scala.Predef.String)
  : resolver.api.AnnotationEntry
  = resolver.impl.AnnotationEntry(
    terminology,
    subject,
    value )
  
  // AnnotationProperty
  
  def createAnnotationProperty
  ( uuid: java.util.UUID,
    iri: gov.nasa.jpl.imce.oml.specification.tables.IRI)
  : resolver.api.AnnotationProperty
  = resolver.impl.AnnotationProperty(
    uuid,
    iri )
  
  // AnnotationPropertyTable
  
  def createAnnotationPropertyTable
  ( key: resolver.api.AnnotationProperty,
    value: scala.collection.immutable.SortedSet[resolver.api.AnnotationEntry])
  : resolver.api.AnnotationPropertyTable
  = resolver.impl.AnnotationPropertyTable(
    key,
    value )
  
  // AnonymousConceptTaxonomyAxiom
  
  def createAnonymousConceptTaxonomyAxiom
  ( uuid: java.util.UUID,
    bundle: resolver.api.Bundle,
    disjointTaxonomyParent: resolver.api.ConceptTreeDisjunction)
  : resolver.api.AnonymousConceptTaxonomyAxiom
  = resolver.impl.AnonymousConceptTaxonomyAxiom(
    uuid,
    bundle,
    disjointTaxonomyParent )
  
  // Aspect
  
  def createAspect
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    name: gov.nasa.jpl.imce.oml.specification.tables.LocalName)
  : resolver.api.Aspect
  = resolver.impl.Aspect(
    graph,
    uuid,
    name )
  
  // AspectSpecializationAxiom
  
  def createAspectSpecializationAxiom
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    subEntity: resolver.api.Entity,
    superAspect: resolver.api.Aspect)
  : resolver.api.AspectSpecializationAxiom
  = resolver.impl.AspectSpecializationAxiom(
    graph,
    uuid,
    subEntity,
    superAspect )
  
  // BinaryScalarRestriction
  
  def createBinaryScalarRestriction
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    name: gov.nasa.jpl.imce.oml.specification.tables.LocalName,
    length: scala.Option[scala.Int],
    maxLength: scala.Option[scala.Int],
    minLength: scala.Option[scala.Int],
    restrictedRange: resolver.api.DataRange)
  : resolver.api.BinaryScalarRestriction
  = resolver.impl.BinaryScalarRestriction(
    graph,
    uuid,
    name,
    length,
    maxLength,
    minLength,
    restrictedRange )
  
  // Bundle
  
  def createBundle
  ( uuid: java.util.UUID,
    kind: gov.nasa.jpl.imce.oml.specification.tables.TerminologyGraphKind,
    name: gov.nasa.jpl.imce.oml.specification.tables.LocalName,
    iri: gov.nasa.jpl.imce.oml.specification.tables.IRI,
    annotations: scala.collection.immutable.SortedSet[resolver.api.Annotation],
    boxStatements: scala.collection.immutable.SortedSet[resolver.api.TerminologyBoxStatement],
    bundleStatements: scala.collection.immutable.SortedSet[resolver.api.TerminologyBundleStatement],
    terminologyBundleAxioms: scala.collection.immutable.SortedSet[resolver.api.TerminologyBundleAxiom])
  : resolver.api.Bundle
  = resolver.impl.Bundle(
    uuid,
    kind,
    name,
    iri,
    annotations,
    boxStatements,
    bundleStatements,
    terminologyBundleAxioms )
  
  // BundledTerminologyAxiom
  
  def createBundledTerminologyAxiom
  ( uuid: java.util.UUID,
    bundledTerminology: resolver.api.TerminologyBox,
    terminologyBundle: resolver.api.Bundle)
  : resolver.api.BundledTerminologyAxiom
  = resolver.impl.BundledTerminologyAxiom(
    uuid,
    bundledTerminology,
    terminologyBundle )
  
  def copyBundledTerminologyAxiom_bundledTerminology
  ( that: resolver.api.BundledTerminologyAxiom,
    bundledTerminology: resolver.api.TerminologyBox )
  : resolver.api.BundledTerminologyAxiom
  = that match {
  	case x: resolver.impl.BundledTerminologyAxiom =>
  	  x.copy(bundledTerminology = bundledTerminology)
  }
  
  def copyBundledTerminologyAxiom_terminologyBundle
  ( that: resolver.api.BundledTerminologyAxiom,
    terminologyBundle: resolver.api.Bundle )
  : resolver.api.BundledTerminologyAxiom
  = that match {
  	case x: resolver.impl.BundledTerminologyAxiom =>
  	  x.copy(terminologyBundle = terminologyBundle)
  }
  
  // Concept
  
  def createConcept
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    isAbstract: scala.Boolean,
    name: gov.nasa.jpl.imce.oml.specification.tables.LocalName)
  : resolver.api.Concept
  = resolver.impl.Concept(
    graph,
    uuid,
    isAbstract,
    name )
  
  // ConceptDesignationTerminologyAxiom
  
  def createConceptDesignationTerminologyAxiom
  ( uuid: java.util.UUID,
    designatedConcept: resolver.api.Concept,
    designatedTerminology: resolver.api.TerminologyBox,
    designationTerminologyGraph: resolver.api.TerminologyGraph)
  : resolver.api.ConceptDesignationTerminologyAxiom
  = resolver.impl.ConceptDesignationTerminologyAxiom(
    uuid,
    designatedConcept,
    designatedTerminology,
    designationTerminologyGraph )
  
  def copyConceptDesignationTerminologyAxiom_designatedTerminology
  ( that: resolver.api.ConceptDesignationTerminologyAxiom,
    designatedTerminology: resolver.api.TerminologyBox )
  : resolver.api.ConceptDesignationTerminologyAxiom
  = that match {
  	case x: resolver.impl.ConceptDesignationTerminologyAxiom =>
  	  x.copy(designatedTerminology = designatedTerminology)
  }
  
  def copyConceptDesignationTerminologyAxiom_designationTerminologyGraph
  ( that: resolver.api.ConceptDesignationTerminologyAxiom,
    designationTerminologyGraph: resolver.api.TerminologyGraph )
  : resolver.api.ConceptDesignationTerminologyAxiom
  = that match {
  	case x: resolver.impl.ConceptDesignationTerminologyAxiom =>
  	  x.copy(designationTerminologyGraph = designationTerminologyGraph)
  }
  
  // ConceptSpecializationAxiom
  
  def createConceptSpecializationAxiom
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    subConcept: resolver.api.Concept,
    superConcept: resolver.api.Concept)
  : resolver.api.ConceptSpecializationAxiom
  = resolver.impl.ConceptSpecializationAxiom(
    graph,
    uuid,
    subConcept,
    superConcept )
  
  // EntityExistentialRestrictionAxiom
  
  def createEntityExistentialRestrictionAxiom
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    restrictedDomain: resolver.api.Entity,
    restrictedRange: resolver.api.Entity,
    restrictedRelation: resolver.api.ReifiedRelationship)
  : resolver.api.EntityExistentialRestrictionAxiom
  = resolver.impl.EntityExistentialRestrictionAxiom(
    graph,
    uuid,
    restrictedDomain,
    restrictedRange,
    restrictedRelation )
  
  // EntityScalarDataProperty
  
  def createEntityScalarDataProperty
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    name: gov.nasa.jpl.imce.oml.specification.tables.LocalName,
    domain: resolver.api.Entity,
    range: resolver.api.DataRange)
  : resolver.api.EntityScalarDataProperty
  = resolver.impl.EntityScalarDataProperty(
    graph,
    uuid,
    name,
    domain,
    range )
  
  // EntityScalarDataPropertyExistentialRestrictionAxiom
  
  def createEntityScalarDataPropertyExistentialRestrictionAxiom
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    restrictedEntity: resolver.api.Entity,
    scalarProperty: resolver.api.EntityScalarDataProperty,
    scalarRestriction: resolver.api.DataRange)
  : resolver.api.EntityScalarDataPropertyExistentialRestrictionAxiom
  = resolver.impl.EntityScalarDataPropertyExistentialRestrictionAxiom(
    graph,
    uuid,
    restrictedEntity,
    scalarProperty,
    scalarRestriction )
  
  // EntityScalarDataPropertyParticularRestrictionAxiom
  
  def createEntityScalarDataPropertyParticularRestrictionAxiom
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    literalValue: gov.nasa.jpl.imce.oml.specification.tables.LexicalValue,
    restrictedEntity: resolver.api.Entity,
    scalarProperty: resolver.api.EntityScalarDataProperty)
  : resolver.api.EntityScalarDataPropertyParticularRestrictionAxiom
  = resolver.impl.EntityScalarDataPropertyParticularRestrictionAxiom(
    graph,
    uuid,
    literalValue,
    restrictedEntity,
    scalarProperty )
  
  // EntityScalarDataPropertyUniversalRestrictionAxiom
  
  def createEntityScalarDataPropertyUniversalRestrictionAxiom
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    restrictedEntity: resolver.api.Entity,
    scalarProperty: resolver.api.EntityScalarDataProperty,
    scalarRestriction: resolver.api.DataRange)
  : resolver.api.EntityScalarDataPropertyUniversalRestrictionAxiom
  = resolver.impl.EntityScalarDataPropertyUniversalRestrictionAxiom(
    graph,
    uuid,
    restrictedEntity,
    scalarProperty,
    scalarRestriction )
  
  // EntityStructuredDataProperty
  
  def createEntityStructuredDataProperty
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    name: gov.nasa.jpl.imce.oml.specification.tables.LocalName,
    domain: resolver.api.Entity,
    range: resolver.api.Structure)
  : resolver.api.EntityStructuredDataProperty
  = resolver.impl.EntityStructuredDataProperty(
    graph,
    uuid,
    name,
    domain,
    range )
  
  // EntityUniversalRestrictionAxiom
  
  def createEntityUniversalRestrictionAxiom
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    restrictedDomain: resolver.api.Entity,
    restrictedRange: resolver.api.Entity,
    restrictedRelation: resolver.api.ReifiedRelationship)
  : resolver.api.EntityUniversalRestrictionAxiom
  = resolver.impl.EntityUniversalRestrictionAxiom(
    graph,
    uuid,
    restrictedDomain,
    restrictedRange,
    restrictedRelation )
  
  // IRIScalarRestriction
  
  def createIRIScalarRestriction
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    name: gov.nasa.jpl.imce.oml.specification.tables.LocalName,
    length: scala.Option[scala.Int],
    maxLength: scala.Option[scala.Int],
    minLength: scala.Option[scala.Int],
    pattern: scala.Option[gov.nasa.jpl.imce.oml.specification.tables.Pattern],
    restrictedRange: resolver.api.DataRange)
  : resolver.api.IRIScalarRestriction
  = resolver.impl.IRIScalarRestriction(
    graph,
    uuid,
    name,
    length,
    maxLength,
    minLength,
    pattern,
    restrictedRange )
  
  // NumericScalarRestriction
  
  def createNumericScalarRestriction
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    name: gov.nasa.jpl.imce.oml.specification.tables.LocalName,
    maxExclusive: scala.Option[gov.nasa.jpl.imce.oml.specification.tables.LexicalNumber],
    maxInclusive: scala.Option[gov.nasa.jpl.imce.oml.specification.tables.LexicalNumber],
    minExclusive: scala.Option[gov.nasa.jpl.imce.oml.specification.tables.LexicalNumber],
    minInclusive: scala.Option[gov.nasa.jpl.imce.oml.specification.tables.LexicalNumber],
    restrictedRange: resolver.api.DataRange)
  : resolver.api.NumericScalarRestriction
  = resolver.impl.NumericScalarRestriction(
    graph,
    uuid,
    name,
    maxExclusive,
    maxInclusive,
    minExclusive,
    minInclusive,
    restrictedRange )
  
  // PlainLiteralScalarRestriction
  
  def createPlainLiteralScalarRestriction
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    name: gov.nasa.jpl.imce.oml.specification.tables.LocalName,
    language: scala.Option[gov.nasa.jpl.imce.oml.specification.tables.Language],
    length: scala.Option[scala.Int],
    maxLength: scala.Option[scala.Int],
    minLength: scala.Option[scala.Int],
    pattern: scala.Option[gov.nasa.jpl.imce.oml.specification.tables.Pattern],
    restrictedRange: resolver.api.DataRange)
  : resolver.api.PlainLiteralScalarRestriction
  = resolver.impl.PlainLiteralScalarRestriction(
    graph,
    uuid,
    name,
    language,
    length,
    maxLength,
    minLength,
    pattern,
    restrictedRange )
  
  // ReifiedRelationship
  
  def createReifiedRelationship
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    isAbstract: scala.Boolean,
    name: gov.nasa.jpl.imce.oml.specification.tables.LocalName,
    unreifiedPropertyName: gov.nasa.jpl.imce.oml.specification.tables.LocalName,
    unreifiedInversePropertyName: scala.Option[gov.nasa.jpl.imce.oml.specification.tables.LocalName],
    isAsymmetric: scala.Boolean,
    isEssential: scala.Boolean,
    isFunctional: scala.Boolean,
    isInverseEssential: scala.Boolean,
    isInverseFunctional: scala.Boolean,
    isIrreflexive: scala.Boolean,
    isReflexive: scala.Boolean,
    isSymmetric: scala.Boolean,
    isTransitive: scala.Boolean,
    source: resolver.api.Entity,
    target: resolver.api.Entity)
  : resolver.api.ReifiedRelationship
  = resolver.impl.ReifiedRelationship(
    graph,
    uuid,
    isAbstract,
    name,
    unreifiedPropertyName,
    unreifiedInversePropertyName,
    isAsymmetric,
    isEssential,
    isFunctional,
    isInverseEssential,
    isInverseFunctional,
    isIrreflexive,
    isReflexive,
    isSymmetric,
    isTransitive,
    source,
    target )
  
  // ReifiedRelationshipSpecializationAxiom
  
  def createReifiedRelationshipSpecializationAxiom
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    subRelationship: resolver.api.ReifiedRelationship,
    superRelationship: resolver.api.ReifiedRelationship)
  : resolver.api.ReifiedRelationshipSpecializationAxiom
  = resolver.impl.ReifiedRelationshipSpecializationAxiom(
    graph,
    uuid,
    subRelationship,
    superRelationship )
  
  // RootConceptTaxonomyAxiom
  
  def createRootConceptTaxonomyAxiom
  ( uuid: java.util.UUID,
    bundle: resolver.api.Bundle,
    root: resolver.api.Concept)
  : resolver.api.RootConceptTaxonomyAxiom
  = resolver.impl.RootConceptTaxonomyAxiom(
    uuid,
    bundle,
    root )
  
  // Scalar
  
  def createScalar
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    name: gov.nasa.jpl.imce.oml.specification.tables.LocalName)
  : resolver.api.Scalar
  = resolver.impl.Scalar(
    graph,
    uuid,
    name )
  
  // ScalarDataProperty
  
  def createScalarDataProperty
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    name: gov.nasa.jpl.imce.oml.specification.tables.LocalName,
    domain: resolver.api.Structure,
    range: resolver.api.DataRange)
  : resolver.api.ScalarDataProperty
  = resolver.impl.ScalarDataProperty(
    graph,
    uuid,
    name,
    domain,
    range )
  
  // ScalarOneOfLiteralAxiom
  
  def createScalarOneOfLiteralAxiom
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    axiom: resolver.api.ScalarOneOfRestriction,
    value: gov.nasa.jpl.imce.oml.specification.tables.LexicalValue)
  : resolver.api.ScalarOneOfLiteralAxiom
  = resolver.impl.ScalarOneOfLiteralAxiom(
    graph,
    uuid,
    axiom,
    value )
  
  // ScalarOneOfRestriction
  
  def createScalarOneOfRestriction
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    name: gov.nasa.jpl.imce.oml.specification.tables.LocalName,
    restrictedRange: resolver.api.DataRange)
  : resolver.api.ScalarOneOfRestriction
  = resolver.impl.ScalarOneOfRestriction(
    graph,
    uuid,
    name,
    restrictedRange )
  
  // SpecificDisjointConceptAxiom
  
  def createSpecificDisjointConceptAxiom
  ( uuid: java.util.UUID,
    bundle: resolver.api.Bundle,
    disjointLeaf: resolver.api.Concept,
    disjointTaxonomyParent: resolver.api.ConceptTreeDisjunction)
  : resolver.api.SpecificDisjointConceptAxiom
  = resolver.impl.SpecificDisjointConceptAxiom(
    uuid,
    bundle,
    disjointLeaf,
    disjointTaxonomyParent )
  
  // StringScalarRestriction
  
  def createStringScalarRestriction
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    name: gov.nasa.jpl.imce.oml.specification.tables.LocalName,
    length: scala.Option[scala.Int],
    maxLength: scala.Option[scala.Int],
    minLength: scala.Option[scala.Int],
    pattern: scala.Option[gov.nasa.jpl.imce.oml.specification.tables.Pattern],
    restrictedRange: resolver.api.DataRange)
  : resolver.api.StringScalarRestriction
  = resolver.impl.StringScalarRestriction(
    graph,
    uuid,
    name,
    length,
    maxLength,
    minLength,
    pattern,
    restrictedRange )
  
  // Structure
  
  def createStructure
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    name: gov.nasa.jpl.imce.oml.specification.tables.LocalName)
  : resolver.api.Structure
  = resolver.impl.Structure(
    graph,
    uuid,
    name )
  
  // StructuredDataProperty
  
  def createStructuredDataProperty
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    name: gov.nasa.jpl.imce.oml.specification.tables.LocalName,
    domain: resolver.api.Structure,
    range: resolver.api.Structure)
  : resolver.api.StructuredDataProperty
  = resolver.impl.StructuredDataProperty(
    graph,
    uuid,
    name,
    domain,
    range )
  
  // SynonymScalarRestriction
  
  def createSynonymScalarRestriction
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    name: gov.nasa.jpl.imce.oml.specification.tables.LocalName,
    restrictedRange: resolver.api.DataRange)
  : resolver.api.SynonymScalarRestriction
  = resolver.impl.SynonymScalarRestriction(
    graph,
    uuid,
    name,
    restrictedRange )
  
  // TerminologyExtensionAxiom
  
  def createTerminologyExtensionAxiom
  ( uuid: java.util.UUID,
    extendedTerminology: resolver.api.TerminologyBox,
    extendingTerminology: resolver.api.TerminologyBox)
  : resolver.api.TerminologyExtensionAxiom
  = resolver.impl.TerminologyExtensionAxiom(
    uuid,
    extendedTerminology,
    extendingTerminology )
  
  def copyTerminologyExtensionAxiom_extendedTerminology
  ( that: resolver.api.TerminologyExtensionAxiom,
    extendedTerminology: resolver.api.TerminologyBox )
  : resolver.api.TerminologyExtensionAxiom
  = that match {
  	case x: resolver.impl.TerminologyExtensionAxiom =>
  	  x.copy(extendedTerminology = extendedTerminology)
  }
  
  def copyTerminologyExtensionAxiom_extendingTerminology
  ( that: resolver.api.TerminologyExtensionAxiom,
    extendingTerminology: resolver.api.TerminologyBox )
  : resolver.api.TerminologyExtensionAxiom
  = that match {
  	case x: resolver.impl.TerminologyExtensionAxiom =>
  	  x.copy(extendingTerminology = extendingTerminology)
  }
  
  // TerminologyGraph
  
  def createTerminologyGraph
  ( uuid: java.util.UUID,
    kind: gov.nasa.jpl.imce.oml.specification.tables.TerminologyGraphKind,
    name: gov.nasa.jpl.imce.oml.specification.tables.LocalName,
    iri: gov.nasa.jpl.imce.oml.specification.tables.IRI,
    annotations: scala.collection.immutable.SortedSet[resolver.api.Annotation],
    boxStatements: scala.collection.immutable.SortedSet[resolver.api.TerminologyBoxStatement])
  : resolver.api.TerminologyGraph
  = resolver.impl.TerminologyGraph(
    uuid,
    kind,
    name,
    iri,
    annotations,
    boxStatements )
  
  // TerminologyNestingAxiom
  
  def createTerminologyNestingAxiom
  ( uuid: java.util.UUID,
    nestedTerminology: resolver.api.TerminologyGraph,
    nestingContext: resolver.api.Concept,
    nestingTerminology: resolver.api.TerminologyBox)
  : resolver.api.TerminologyNestingAxiom
  = resolver.impl.TerminologyNestingAxiom(
    uuid,
    nestedTerminology,
    nestingContext,
    nestingTerminology )
  
  def copyTerminologyNestingAxiom_nestedTerminology
  ( that: resolver.api.TerminologyNestingAxiom,
    nestedTerminology: resolver.api.TerminologyGraph )
  : resolver.api.TerminologyNestingAxiom
  = that match {
  	case x: resolver.impl.TerminologyNestingAxiom =>
  	  x.copy(nestedTerminology = nestedTerminology)
  }
  
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
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    name: gov.nasa.jpl.imce.oml.specification.tables.LocalName,
    maxExclusive: scala.Option[gov.nasa.jpl.imce.oml.specification.tables.LexicalTime],
    maxInclusive: scala.Option[gov.nasa.jpl.imce.oml.specification.tables.LexicalTime],
    minExclusive: scala.Option[gov.nasa.jpl.imce.oml.specification.tables.LexicalTime],
    minInclusive: scala.Option[gov.nasa.jpl.imce.oml.specification.tables.LexicalTime],
    restrictedRange: resolver.api.DataRange)
  : resolver.api.TimeScalarRestriction
  = resolver.impl.TimeScalarRestriction(
    graph,
    uuid,
    name,
    maxExclusive,
    maxInclusive,
    minExclusive,
    minInclusive,
    restrictedRange )
  
  // UnreifiedRelationship
  
  def createUnreifiedRelationship
  ( graph: resolver.api.TerminologyBox,
    uuid: java.util.UUID,
    name: gov.nasa.jpl.imce.oml.specification.tables.LocalName,
    isAsymmetric: scala.Boolean,
    isEssential: scala.Boolean,
    isFunctional: scala.Boolean,
    isInverseEssential: scala.Boolean,
    isInverseFunctional: scala.Boolean,
    isIrreflexive: scala.Boolean,
    isReflexive: scala.Boolean,
    isSymmetric: scala.Boolean,
    isTransitive: scala.Boolean,
    source: resolver.api.Entity,
    target: resolver.api.Entity)
  : resolver.api.UnreifiedRelationship
  = resolver.impl.UnreifiedRelationship(
    graph,
    uuid,
    name,
    isAsymmetric,
    isEssential,
    isFunctional,
    isInverseEssential,
    isInverseFunctional,
    isIrreflexive,
    isReflexive,
    isSymmetric,
    isTransitive,
    source,
    target )
  
}
