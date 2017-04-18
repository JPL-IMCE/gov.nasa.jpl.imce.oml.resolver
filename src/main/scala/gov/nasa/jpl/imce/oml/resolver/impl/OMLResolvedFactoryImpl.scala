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

import scala.Predef.ArrowAssoc

case class OMLResolvedFactoryImpl
( override val oug: uuid.OMLUUIDGenerator ) 
extends resolver.api.OMLResolvedFactory {
	
  override def createExtent
  : resolver.api.Extent 
  = resolver.api.Extent()
  
  // Annotation
  override def createAnnotation
  ( extent: resolver.api.Extent,
    module: resolver.api.Module,
    subject: resolver.api.Element,
    property: resolver.api.AnnotationProperty,
    value: scala.Predef.String )
  : (resolver.api.Extent, resolver.api.Annotation)
  = {
    // factoryMethodWithoutUUID
    // container: module Module
    // contained: annotations Annotation
    val annotation = Annotation( subject, property, value )
    scala.Tuple2(
      extent.copy(
        annotations = extent.withAnnotation(module, annotation),
        moduleOfAnnotation = extent.moduleOfAnnotation + (annotation -> module)),
      annotation)
  }
  		  
  // AnnotationEntry
  override def createAnnotationEntry
  ( extent: resolver.api.Extent,
    module: resolver.api.Module,
    subject: resolver.api.Element,
    value: scala.Predef.String )
  : (resolver.api.Extent, resolver.api.AnnotationEntry)
  = scala.Tuple2(
  	extent, 
   	AnnotationEntry( module, subject, value )
  )
  		  
  // AnnotationProperty
  override def createAnnotationProperty
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    iri: gov.nasa.jpl.imce.oml.tables.IRI,
    abbrevIRI: gov.nasa.jpl.imce.oml.tables.AbbrevIRI )
  : (resolver.api.Extent, resolver.api.AnnotationProperty)
  = {
    val annotationProperty = AnnotationProperty( uuid, iri, abbrevIRI )
    scala.Tuple2(
  	extent.copy(annotationProperties = extent.annotationProperties + (uuid -> annotationProperty)), 
   	annotationProperty)
  }
  		  
  // AnonymousConceptTaxonomyAxiom
  override def createAnonymousConceptTaxonomyAxiom
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    disjointTaxonomyParent: resolver.api.ConceptTreeDisjunction,
    name: gov.nasa.jpl.imce.oml.tables.LocalName )
  : (resolver.api.Extent, resolver.api.AnonymousConceptTaxonomyAxiom)
  = {
    // factoryMethodWithUUIDGenerator
    // container: disjointTaxonomyParent ConceptTreeDisjunction
    // contained: disjunctions DisjointUnionOfConceptsAxiom
    val anonymousConceptTaxonomyAxiom = AnonymousConceptTaxonomyAxiom( uuid, name )
    scala.Tuple2(
      extent.copy(
    	  disjunctions = extent.withDisjointUnionOfConceptsAxiom(disjointTaxonomyParent, anonymousConceptTaxonomyAxiom),
    	  conceptTreeDisjunctionOfDisjointUnionOfConceptsAxiom = extent.conceptTreeDisjunctionOfDisjointUnionOfConceptsAxiom + (anonymousConceptTaxonomyAxiom -> disjointTaxonomyParent),
    	  disjointUnionOfConceptsAxiomByUUID = extent.disjointUnionOfConceptsAxiomByUUID + (uuid -> anonymousConceptTaxonomyAxiom)),
    	anonymousConceptTaxonomyAxiom)
  }
  		  
  // Aspect
  override def createAspect
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    name: gov.nasa.jpl.imce.oml.tables.LocalName )
  : (resolver.api.Extent, resolver.api.Aspect)
  = {
    // factoryMethodWithUUIDGenerator
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val aspect = Aspect( uuid, name )
    scala.Tuple2(
      extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, aspect),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (aspect -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> aspect)),
    	aspect)
  }
  		  
  // AspectSpecializationAxiom
  override def createAspectSpecializationAxiom
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    superAspect: resolver.api.Aspect,
    subEntity: resolver.api.Entity )
  : (resolver.api.Extent, resolver.api.AspectSpecializationAxiom)
  = {
    // factoryMethodWithDerivedUUID
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val aspectSpecializationAxiom = AspectSpecializationAxiom( uuid, superAspect, subEntity )
    scala.Tuple2(
    	extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, aspectSpecializationAxiom),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (aspectSpecializationAxiom -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> aspectSpecializationAxiom)),
    	aspectSpecializationAxiom)
  }
  		  
  // BinaryScalarRestriction
  override def createBinaryScalarRestriction
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    restrictedRange: resolver.api.DataRange,
    length: scala.Option[scala.Int],
    minLength: scala.Option[scala.Int],
    maxLength: scala.Option[scala.Int],
    name: gov.nasa.jpl.imce.oml.tables.LocalName )
  : (resolver.api.Extent, resolver.api.BinaryScalarRestriction)
  = {
    // factoryMethodWithUUIDGenerator
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val binaryScalarRestriction = BinaryScalarRestriction( uuid, restrictedRange, length, minLength, maxLength, name )
    scala.Tuple2(
      extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, binaryScalarRestriction),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (binaryScalarRestriction -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> binaryScalarRestriction)),
    	binaryScalarRestriction)
  }
  		  
  // Bundle
  override def createBundle
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    kind: gov.nasa.jpl.imce.oml.tables.TerminologyKind,
    iri: gov.nasa.jpl.imce.oml.tables.IRI )
  : (resolver.api.Extent, resolver.api.Bundle)
  = {
    val bundle = Bundle( uuid, kind, iri )
    scala.Tuple2(
  	extent.copy(bundles = extent.bundles + (uuid -> bundle)), 
   	bundle)
  }
  		  
  // BundledTerminologyAxiom
  override def createBundledTerminologyAxiom
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    bundledTerminology: resolver.api.TerminologyBox,
    bundle: resolver.api.Bundle )
  : (resolver.api.Extent, resolver.api.BundledTerminologyAxiom)
  = {
    // factoryMethodWithDerivedUUID
    // container: bundle Bundle
    // contained: bundleAxioms TerminologyBundleAxiom
    val bundledTerminologyAxiom = BundledTerminologyAxiom( uuid, bundledTerminology )
    scala.Tuple2(
    	extent.copy(
    	  bundleAxioms = extent.withTerminologyBundleAxiom(bundle, bundledTerminologyAxiom),
    	  bundleOfTerminologyBundleAxiom = extent.bundleOfTerminologyBundleAxiom + (bundledTerminologyAxiom -> bundle),
    	  terminologyBundleAxiomByUUID = extent.terminologyBundleAxiomByUUID + (uuid -> bundledTerminologyAxiom)),
    	bundledTerminologyAxiom)
  }
  		  
  // Concept
  override def createConcept
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    name: gov.nasa.jpl.imce.oml.tables.LocalName )
  : (resolver.api.Extent, resolver.api.Concept)
  = {
    // factoryMethodWithUUIDGenerator
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val concept = Concept( uuid, name )
    scala.Tuple2(
      extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, concept),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (concept -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> concept)),
    	concept)
  }
  		  
  // ConceptDesignationTerminologyAxiom
  override def createConceptDesignationTerminologyAxiom
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    designatedConcept: resolver.api.Concept,
    designatedTerminology: resolver.api.TerminologyBox )
  : (resolver.api.Extent, resolver.api.ConceptDesignationTerminologyAxiom)
  = {
    // factoryMethodWithDerivedUUID
    // container: tbox TerminologyBox
    // contained: boxAxioms TerminologyBoxAxiom
    val conceptDesignationTerminologyAxiom = ConceptDesignationTerminologyAxiom( uuid, designatedConcept, designatedTerminology )
    scala.Tuple2(
    	extent.copy(
    	  boxAxioms = extent.withTerminologyBoxAxiom(tbox, conceptDesignationTerminologyAxiom),
    	  terminologyBoxOfTerminologyBoxAxiom = extent.terminologyBoxOfTerminologyBoxAxiom + (conceptDesignationTerminologyAxiom -> tbox),
    	  terminologyBoxAxiomByUUID = extent.terminologyBoxAxiomByUUID + (uuid -> conceptDesignationTerminologyAxiom)),
    	conceptDesignationTerminologyAxiom)
  }
  		  
  // ConceptInstance
  override def createConceptInstance
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    descriptionBox: resolver.api.DescriptionBox,
    singletonConceptClassifier: resolver.api.Concept,
    name: gov.nasa.jpl.imce.oml.tables.LocalName )
  : (resolver.api.Extent, resolver.api.ConceptInstance)
  = {
    // factoryMethodWithUUIDGenerator
    // container: descriptionBox DescriptionBox
    // contained: conceptInstances ConceptInstance
    val conceptInstance = ConceptInstance( uuid, singletonConceptClassifier, name )
    scala.Tuple2(
      extent.copy(
    	  conceptInstances = extent.withConceptInstance(descriptionBox, conceptInstance),
    	  descriptionBoxOfConceptInstance = extent.descriptionBoxOfConceptInstance + (conceptInstance -> descriptionBox),
    	  conceptInstanceByUUID = extent.conceptInstanceByUUID + (uuid -> conceptInstance)),
    	conceptInstance)
  }
  		  
  // ConceptSpecializationAxiom
  override def createConceptSpecializationAxiom
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    superConcept: resolver.api.Concept,
    subConcept: resolver.api.Concept )
  : (resolver.api.Extent, resolver.api.ConceptSpecializationAxiom)
  = {
    // factoryMethodWithDerivedUUID
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val conceptSpecializationAxiom = ConceptSpecializationAxiom( uuid, superConcept, subConcept )
    scala.Tuple2(
    	extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, conceptSpecializationAxiom),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (conceptSpecializationAxiom -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> conceptSpecializationAxiom)),
    	conceptSpecializationAxiom)
  }
  		  
  // DataStructureTuple
  override def createDataStructureTuple
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    dataStructureType: resolver.api.Structure,
    structuredDataPropertyValue: resolver.api.StructuredDataPropertyValue )
  : (resolver.api.Extent, resolver.api.DataStructureTuple)
  = {
    // factoryMethodWithDerivedUUID
    // container: structuredDataPropertyValue StructuredDataPropertyValue
    // contained: structuredPropertyTuple DataStructureTuple
    val dataStructureTuple = DataStructureTuple( uuid, dataStructureType )
    scala.Tuple2(
    	extent.copy(
    	  structuredPropertyTuple = extent.withDataStructureTuple(structuredDataPropertyValue, dataStructureTuple),
    	  structuredDataPropertyValueOfDataStructureTuple = extent.structuredDataPropertyValueOfDataStructureTuple + (dataStructureTuple -> structuredDataPropertyValue),
    	  dataStructureTupleByUUID = extent.dataStructureTupleByUUID + (uuid -> dataStructureTuple)),
    	dataStructureTuple)
  }
  		  
  // DescriptionBox
  override def createDescriptionBox
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    kind: gov.nasa.jpl.imce.oml.tables.DescriptionKind,
    iri: gov.nasa.jpl.imce.oml.tables.IRI )
  : (resolver.api.Extent, resolver.api.DescriptionBox)
  = {
    val descriptionBox = DescriptionBox( uuid, kind, iri )
    scala.Tuple2(
  	extent.copy(descriptionBoxes = extent.descriptionBoxes + (uuid -> descriptionBox)), 
   	descriptionBox)
  }
  		  
  // DescriptionBoxExtendsClosedWorldDefinitions
  override def createDescriptionBoxExtendsClosedWorldDefinitions
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    descriptionBox: resolver.api.DescriptionBox,
    closedWorldDefinitions: resolver.api.TerminologyBox )
  : (resolver.api.Extent, resolver.api.DescriptionBoxExtendsClosedWorldDefinitions)
  = {
    // factoryMethodWithDerivedUUID
    // container: descriptionBox DescriptionBox
    // contained: closedWorldDefinitions DescriptionBoxExtendsClosedWorldDefinitions
    val descriptionBoxExtendsClosedWorldDefinitions = DescriptionBoxExtendsClosedWorldDefinitions( uuid, closedWorldDefinitions )
    scala.Tuple2(
    	extent.copy(
    	  closedWorldDefinitions = extent.withDescriptionBoxExtendsClosedWorldDefinitions(descriptionBox, descriptionBoxExtendsClosedWorldDefinitions),
    	  descriptionBoxOfDescriptionBoxExtendsClosedWorldDefinitions = extent.descriptionBoxOfDescriptionBoxExtendsClosedWorldDefinitions + (descriptionBoxExtendsClosedWorldDefinitions -> descriptionBox),
    	  descriptionBoxExtendsClosedWorldDefinitionsByUUID = extent.descriptionBoxExtendsClosedWorldDefinitionsByUUID + (uuid -> descriptionBoxExtendsClosedWorldDefinitions)),
    	descriptionBoxExtendsClosedWorldDefinitions)
  }
  		  
  // DescriptionBoxRefinement
  override def createDescriptionBoxRefinement
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    refiningDescriptionBox: resolver.api.DescriptionBox,
    refinedDescriptionBox: resolver.api.DescriptionBox )
  : (resolver.api.Extent, resolver.api.DescriptionBoxRefinement)
  = {
    // factoryMethodWithDerivedUUID
    // container: refiningDescriptionBox DescriptionBox
    // contained: descriptionBoxRefinements DescriptionBoxRefinement
    val descriptionBoxRefinement = DescriptionBoxRefinement( uuid, refinedDescriptionBox )
    scala.Tuple2(
    	extent.copy(
    	  descriptionBoxRefinements = extent.withDescriptionBoxRefinement(refiningDescriptionBox, descriptionBoxRefinement),
    	  descriptionBoxOfDescriptionBoxRefinement = extent.descriptionBoxOfDescriptionBoxRefinement + (descriptionBoxRefinement -> refiningDescriptionBox),
    	  descriptionBoxRefinementByUUID = extent.descriptionBoxRefinementByUUID + (uuid -> descriptionBoxRefinement)),
    	descriptionBoxRefinement)
  }
  		  
  // EntityExistentialRestrictionAxiom
  override def createEntityExistentialRestrictionAxiom
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    restrictedRelation: resolver.api.EntityRelationship,
    restrictedDomain: resolver.api.Entity,
    restrictedRange: resolver.api.Entity )
  : (resolver.api.Extent, resolver.api.EntityExistentialRestrictionAxiom)
  = {
    // factoryMethodWithDerivedUUID
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val entityExistentialRestrictionAxiom = EntityExistentialRestrictionAxiom( uuid, restrictedRelation, restrictedDomain, restrictedRange )
    scala.Tuple2(
    	extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, entityExistentialRestrictionAxiom),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (entityExistentialRestrictionAxiom -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> entityExistentialRestrictionAxiom)),
    	entityExistentialRestrictionAxiom)
  }
  		  
  // EntityScalarDataProperty
  override def createEntityScalarDataProperty
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    domain: resolver.api.Entity,
    range: resolver.api.DataRange,
    isIdentityCriteria: scala.Boolean,
    name: gov.nasa.jpl.imce.oml.tables.LocalName )
  : (resolver.api.Extent, resolver.api.EntityScalarDataProperty)
  = {
    // factoryMethodWithUUIDGenerator
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val entityScalarDataProperty = EntityScalarDataProperty( uuid, domain, range, isIdentityCriteria, name )
    scala.Tuple2(
      extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, entityScalarDataProperty),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (entityScalarDataProperty -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> entityScalarDataProperty)),
    	entityScalarDataProperty)
  }
  		  
  // EntityScalarDataPropertyExistentialRestrictionAxiom
  override def createEntityScalarDataPropertyExistentialRestrictionAxiom
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    restrictedEntity: resolver.api.Entity,
    scalarProperty: resolver.api.EntityScalarDataProperty,
    scalarRestriction: resolver.api.DataRange )
  : (resolver.api.Extent, resolver.api.EntityScalarDataPropertyExistentialRestrictionAxiom)
  = {
    // factoryMethodWithDerivedUUID
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val entityScalarDataPropertyExistentialRestrictionAxiom = EntityScalarDataPropertyExistentialRestrictionAxiom( uuid, restrictedEntity, scalarProperty, scalarRestriction )
    scala.Tuple2(
    	extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, entityScalarDataPropertyExistentialRestrictionAxiom),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (entityScalarDataPropertyExistentialRestrictionAxiom -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> entityScalarDataPropertyExistentialRestrictionAxiom)),
    	entityScalarDataPropertyExistentialRestrictionAxiom)
  }
  		  
  // EntityScalarDataPropertyParticularRestrictionAxiom
  override def createEntityScalarDataPropertyParticularRestrictionAxiom
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    restrictedEntity: resolver.api.Entity,
    scalarProperty: resolver.api.EntityScalarDataProperty,
    literalValue: gov.nasa.jpl.imce.oml.tables.LexicalValue )
  : (resolver.api.Extent, resolver.api.EntityScalarDataPropertyParticularRestrictionAxiom)
  = {
    // factoryMethodWithDerivedUUID
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val entityScalarDataPropertyParticularRestrictionAxiom = EntityScalarDataPropertyParticularRestrictionAxiom( uuid, restrictedEntity, scalarProperty, literalValue )
    scala.Tuple2(
    	extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, entityScalarDataPropertyParticularRestrictionAxiom),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (entityScalarDataPropertyParticularRestrictionAxiom -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> entityScalarDataPropertyParticularRestrictionAxiom)),
    	entityScalarDataPropertyParticularRestrictionAxiom)
  }
  		  
  // EntityScalarDataPropertyUniversalRestrictionAxiom
  override def createEntityScalarDataPropertyUniversalRestrictionAxiom
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    restrictedEntity: resolver.api.Entity,
    scalarProperty: resolver.api.EntityScalarDataProperty,
    scalarRestriction: resolver.api.DataRange )
  : (resolver.api.Extent, resolver.api.EntityScalarDataPropertyUniversalRestrictionAxiom)
  = {
    // factoryMethodWithDerivedUUID
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val entityScalarDataPropertyUniversalRestrictionAxiom = EntityScalarDataPropertyUniversalRestrictionAxiom( uuid, restrictedEntity, scalarProperty, scalarRestriction )
    scala.Tuple2(
    	extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, entityScalarDataPropertyUniversalRestrictionAxiom),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (entityScalarDataPropertyUniversalRestrictionAxiom -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> entityScalarDataPropertyUniversalRestrictionAxiom)),
    	entityScalarDataPropertyUniversalRestrictionAxiom)
  }
  		  
  // EntityStructuredDataProperty
  override def createEntityStructuredDataProperty
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    domain: resolver.api.Entity,
    range: resolver.api.Structure,
    isIdentityCriteria: scala.Boolean,
    name: gov.nasa.jpl.imce.oml.tables.LocalName )
  : (resolver.api.Extent, resolver.api.EntityStructuredDataProperty)
  = {
    // factoryMethodWithUUIDGenerator
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val entityStructuredDataProperty = EntityStructuredDataProperty( uuid, domain, range, isIdentityCriteria, name )
    scala.Tuple2(
      extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, entityStructuredDataProperty),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (entityStructuredDataProperty -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> entityStructuredDataProperty)),
    	entityStructuredDataProperty)
  }
  		  
  // EntityUniversalRestrictionAxiom
  override def createEntityUniversalRestrictionAxiom
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    restrictedRelation: resolver.api.EntityRelationship,
    restrictedDomain: resolver.api.Entity,
    restrictedRange: resolver.api.Entity )
  : (resolver.api.Extent, resolver.api.EntityUniversalRestrictionAxiom)
  = {
    // factoryMethodWithDerivedUUID
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val entityUniversalRestrictionAxiom = EntityUniversalRestrictionAxiom( uuid, restrictedRelation, restrictedDomain, restrictedRange )
    scala.Tuple2(
    	extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, entityUniversalRestrictionAxiom),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (entityUniversalRestrictionAxiom -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> entityUniversalRestrictionAxiom)),
    	entityUniversalRestrictionAxiom)
  }
  		  
  // IRIScalarRestriction
  override def createIRIScalarRestriction
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    restrictedRange: resolver.api.DataRange,
    length: scala.Option[scala.Int],
    minLength: scala.Option[scala.Int],
    maxLength: scala.Option[scala.Int],
    name: gov.nasa.jpl.imce.oml.tables.LocalName,
    pattern: scala.Option[gov.nasa.jpl.imce.oml.tables.Pattern] )
  : (resolver.api.Extent, resolver.api.IRIScalarRestriction)
  = {
    // factoryMethodWithUUIDGenerator
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val iRIScalarRestriction = IRIScalarRestriction( uuid, restrictedRange, length, minLength, maxLength, name, pattern )
    scala.Tuple2(
      extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, iRIScalarRestriction),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (iRIScalarRestriction -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> iRIScalarRestriction)),
    	iRIScalarRestriction)
  }
  		  
  // NumericScalarRestriction
  override def createNumericScalarRestriction
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    restrictedRange: resolver.api.DataRange,
    minExclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalNumber],
    minInclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalNumber],
    maxExclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalNumber],
    maxInclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalNumber],
    name: gov.nasa.jpl.imce.oml.tables.LocalName )
  : (resolver.api.Extent, resolver.api.NumericScalarRestriction)
  = {
    // factoryMethodWithUUIDGenerator
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val numericScalarRestriction = NumericScalarRestriction( uuid, restrictedRange, minExclusive, minInclusive, maxExclusive, maxInclusive, name )
    scala.Tuple2(
      extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, numericScalarRestriction),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (numericScalarRestriction -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> numericScalarRestriction)),
    	numericScalarRestriction)
  }
  		  
  // PlainLiteralScalarRestriction
  override def createPlainLiteralScalarRestriction
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    restrictedRange: resolver.api.DataRange,
    length: scala.Option[scala.Int],
    minLength: scala.Option[scala.Int],
    maxLength: scala.Option[scala.Int],
    name: gov.nasa.jpl.imce.oml.tables.LocalName,
    langRange: scala.Option[gov.nasa.jpl.imce.oml.tables.LangRange],
    pattern: scala.Option[gov.nasa.jpl.imce.oml.tables.Pattern] )
  : (resolver.api.Extent, resolver.api.PlainLiteralScalarRestriction)
  = {
    // factoryMethodWithUUIDGenerator
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val plainLiteralScalarRestriction = PlainLiteralScalarRestriction( uuid, restrictedRange, length, minLength, maxLength, name, langRange, pattern )
    scala.Tuple2(
      extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, plainLiteralScalarRestriction),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (plainLiteralScalarRestriction -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> plainLiteralScalarRestriction)),
    	plainLiteralScalarRestriction)
  }
  		  
  // ReifiedRelationship
  override def createReifiedRelationship
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
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
    unreifiedInversePropertyName: scala.Option[gov.nasa.jpl.imce.oml.tables.LocalName] )
  : (resolver.api.Extent, resolver.api.ReifiedRelationship)
  = {
    // factoryMethodWithUUIDGenerator
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val reifiedRelationship = ReifiedRelationship( uuid, source, target, isAsymmetric, isEssential, isFunctional, isInverseEssential, isInverseFunctional, isIrreflexive, isReflexive, isSymmetric, isTransitive, name, unreifiedPropertyName, unreifiedInversePropertyName )
    scala.Tuple2(
      extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, reifiedRelationship),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (reifiedRelationship -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> reifiedRelationship)),
    	reifiedRelationship)
  }
  		  
  // ReifiedRelationshipInstance
  override def createReifiedRelationshipInstance
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    descriptionBox: resolver.api.DescriptionBox,
    singletonReifiedRelationshipClassifier: resolver.api.ReifiedRelationship,
    name: gov.nasa.jpl.imce.oml.tables.LocalName )
  : (resolver.api.Extent, resolver.api.ReifiedRelationshipInstance)
  = {
    // factoryMethodWithUUIDGenerator
    // container: descriptionBox DescriptionBox
    // contained: reifiedRelationshipInstances ReifiedRelationshipInstance
    val reifiedRelationshipInstance = ReifiedRelationshipInstance( uuid, singletonReifiedRelationshipClassifier, name )
    scala.Tuple2(
      extent.copy(
    	  reifiedRelationshipInstances = extent.withReifiedRelationshipInstance(descriptionBox, reifiedRelationshipInstance),
    	  descriptionBoxOfReifiedRelationshipInstance = extent.descriptionBoxOfReifiedRelationshipInstance + (reifiedRelationshipInstance -> descriptionBox),
    	  reifiedRelationshipInstanceByUUID = extent.reifiedRelationshipInstanceByUUID + (uuid -> reifiedRelationshipInstance)),
    	reifiedRelationshipInstance)
  }
  		  
  // ReifiedRelationshipInstanceDomain
  override def createReifiedRelationshipInstanceDomain
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    descriptionBox: resolver.api.DescriptionBox,
    reifiedRelationshipInstance: resolver.api.ReifiedRelationshipInstance,
    domain: resolver.api.ConceptualEntitySingletonInstance )
  : (resolver.api.Extent, resolver.api.ReifiedRelationshipInstanceDomain)
  = {
    // factoryMethodWithDerivedUUID
    // container: descriptionBox DescriptionBox
    // contained: reifiedRelationshipInstanceDomains ReifiedRelationshipInstanceDomain
    val reifiedRelationshipInstanceDomain = ReifiedRelationshipInstanceDomain( uuid, reifiedRelationshipInstance, domain )
    scala.Tuple2(
    	extent.copy(
    	  reifiedRelationshipInstanceDomains = extent.withReifiedRelationshipInstanceDomain(descriptionBox, reifiedRelationshipInstanceDomain),
    	  descriptionBoxOfReifiedRelationshipInstanceDomain = extent.descriptionBoxOfReifiedRelationshipInstanceDomain + (reifiedRelationshipInstanceDomain -> descriptionBox),
    	  reifiedRelationshipInstanceDomainByUUID = extent.reifiedRelationshipInstanceDomainByUUID + (uuid -> reifiedRelationshipInstanceDomain)),
    	reifiedRelationshipInstanceDomain)
  }
  		  
  // ReifiedRelationshipInstanceRange
  override def createReifiedRelationshipInstanceRange
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    descriptionBox: resolver.api.DescriptionBox,
    reifiedRelationshipInstance: resolver.api.ReifiedRelationshipInstance,
    range: resolver.api.ConceptualEntitySingletonInstance )
  : (resolver.api.Extent, resolver.api.ReifiedRelationshipInstanceRange)
  = {
    // factoryMethodWithDerivedUUID
    // container: descriptionBox DescriptionBox
    // contained: reifiedRelationshipInstanceRanges ReifiedRelationshipInstanceRange
    val reifiedRelationshipInstanceRange = ReifiedRelationshipInstanceRange( uuid, reifiedRelationshipInstance, range )
    scala.Tuple2(
    	extent.copy(
    	  reifiedRelationshipInstanceRanges = extent.withReifiedRelationshipInstanceRange(descriptionBox, reifiedRelationshipInstanceRange),
    	  descriptionBoxOfReifiedRelationshipInstanceRange = extent.descriptionBoxOfReifiedRelationshipInstanceRange + (reifiedRelationshipInstanceRange -> descriptionBox),
    	  reifiedRelationshipInstanceRangeByUUID = extent.reifiedRelationshipInstanceRangeByUUID + (uuid -> reifiedRelationshipInstanceRange)),
    	reifiedRelationshipInstanceRange)
  }
  		  
  // ReifiedRelationshipSpecializationAxiom
  override def createReifiedRelationshipSpecializationAxiom
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    superRelationship: resolver.api.ReifiedRelationship,
    subRelationship: resolver.api.ReifiedRelationship )
  : (resolver.api.Extent, resolver.api.ReifiedRelationshipSpecializationAxiom)
  = {
    // factoryMethodWithDerivedUUID
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val reifiedRelationshipSpecializationAxiom = ReifiedRelationshipSpecializationAxiom( uuid, superRelationship, subRelationship )
    scala.Tuple2(
    	extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, reifiedRelationshipSpecializationAxiom),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (reifiedRelationshipSpecializationAxiom -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> reifiedRelationshipSpecializationAxiom)),
    	reifiedRelationshipSpecializationAxiom)
  }
  		  
  // RootConceptTaxonomyAxiom
  override def createRootConceptTaxonomyAxiom
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    bundle: resolver.api.Bundle,
    root: resolver.api.Concept )
  : (resolver.api.Extent, resolver.api.RootConceptTaxonomyAxiom)
  = {
    // factoryMethodWithDerivedUUID
    // container: bundle Bundle
    // contained: bundleStatements TerminologyBundleStatement
    val rootConceptTaxonomyAxiom = RootConceptTaxonomyAxiom( uuid, root )
    scala.Tuple2(
    	extent.copy(
    	  bundleStatements = extent.withTerminologyBundleStatement(bundle, rootConceptTaxonomyAxiom),
    	  bundleOfTerminologyBundleStatement = extent.bundleOfTerminologyBundleStatement + (rootConceptTaxonomyAxiom -> bundle),
    	  terminologyBundleStatementByUUID = extent.terminologyBundleStatementByUUID + (uuid -> rootConceptTaxonomyAxiom)),
    	rootConceptTaxonomyAxiom)
  }
  		  
  // Scalar
  override def createScalar
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    name: gov.nasa.jpl.imce.oml.tables.LocalName )
  : (resolver.api.Extent, resolver.api.Scalar)
  = {
    // factoryMethodWithUUIDGenerator
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val scalar = Scalar( uuid, name )
    scala.Tuple2(
      extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, scalar),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (scalar -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> scalar)),
    	scalar)
  }
  		  
  // ScalarDataProperty
  override def createScalarDataProperty
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    domain: resolver.api.Structure,
    range: resolver.api.DataRange,
    name: gov.nasa.jpl.imce.oml.tables.LocalName )
  : (resolver.api.Extent, resolver.api.ScalarDataProperty)
  = {
    // factoryMethodWithUUIDGenerator
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val scalarDataProperty = ScalarDataProperty( uuid, domain, range, name )
    scala.Tuple2(
      extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, scalarDataProperty),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (scalarDataProperty -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> scalarDataProperty)),
    	scalarDataProperty)
  }
  		  
  // ScalarDataPropertyValue
  override def createScalarDataPropertyValue
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    singletonInstance: resolver.api.SingletonInstance,
    scalarDataProperty: resolver.api.DataRelationshipToScalar,
    scalarPropertyValue: scala.Predef.String )
  : (resolver.api.Extent, resolver.api.ScalarDataPropertyValue)
  = {
    // factoryMethodWithDerivedUUID
    // container: singletonInstance SingletonInstance
    // contained: scalarDataPropertyValues ScalarDataPropertyValue
    val scalarDataPropertyValue = ScalarDataPropertyValue( uuid, scalarDataProperty, scalarPropertyValue )
    scala.Tuple2(
    	extent.copy(
    	  scalarDataPropertyValues = extent.withScalarDataPropertyValue(singletonInstance, scalarDataPropertyValue),
    	  singletonInstanceOfScalarDataPropertyValue = extent.singletonInstanceOfScalarDataPropertyValue + (scalarDataPropertyValue -> singletonInstance),
    	  scalarDataPropertyValueByUUID = extent.scalarDataPropertyValueByUUID + (uuid -> scalarDataPropertyValue)),
    	scalarDataPropertyValue)
  }
  		  
  // ScalarOneOfLiteralAxiom
  override def createScalarOneOfLiteralAxiom
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    axiom: resolver.api.ScalarOneOfRestriction,
    value: gov.nasa.jpl.imce.oml.tables.LexicalValue )
  : (resolver.api.Extent, resolver.api.ScalarOneOfLiteralAxiom)
  = {
    // factoryMethodWithImplicitlyDerivedUUID
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val scalarOneOfLiteralAxiom = ScalarOneOfLiteralAxiom( uuid, axiom, value )
    scala.Tuple2(
    	extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, scalarOneOfLiteralAxiom),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (scalarOneOfLiteralAxiom -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> scalarOneOfLiteralAxiom)),
    	scalarOneOfLiteralAxiom)
  }
  		  
  // ScalarOneOfRestriction
  override def createScalarOneOfRestriction
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    restrictedRange: resolver.api.DataRange,
    name: gov.nasa.jpl.imce.oml.tables.LocalName )
  : (resolver.api.Extent, resolver.api.ScalarOneOfRestriction)
  = {
    // factoryMethodWithUUIDGenerator
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val scalarOneOfRestriction = ScalarOneOfRestriction( uuid, restrictedRange, name )
    scala.Tuple2(
      extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, scalarOneOfRestriction),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (scalarOneOfRestriction -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> scalarOneOfRestriction)),
    	scalarOneOfRestriction)
  }
  		  
  // SpecificDisjointConceptAxiom
  override def createSpecificDisjointConceptAxiom
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    disjointTaxonomyParent: resolver.api.ConceptTreeDisjunction,
    disjointLeaf: resolver.api.Concept )
  : (resolver.api.Extent, resolver.api.SpecificDisjointConceptAxiom)
  = {
    // factoryMethodWithDerivedUUID
    // container: disjointTaxonomyParent ConceptTreeDisjunction
    // contained: disjunctions DisjointUnionOfConceptsAxiom
    val specificDisjointConceptAxiom = SpecificDisjointConceptAxiom( uuid, disjointLeaf )
    scala.Tuple2(
    	extent.copy(
    	  disjunctions = extent.withDisjointUnionOfConceptsAxiom(disjointTaxonomyParent, specificDisjointConceptAxiom),
    	  conceptTreeDisjunctionOfDisjointUnionOfConceptsAxiom = extent.conceptTreeDisjunctionOfDisjointUnionOfConceptsAxiom + (specificDisjointConceptAxiom -> disjointTaxonomyParent),
    	  disjointUnionOfConceptsAxiomByUUID = extent.disjointUnionOfConceptsAxiomByUUID + (uuid -> specificDisjointConceptAxiom)),
    	specificDisjointConceptAxiom)
  }
  		  
  // StringScalarRestriction
  override def createStringScalarRestriction
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    restrictedRange: resolver.api.DataRange,
    length: scala.Option[scala.Int],
    minLength: scala.Option[scala.Int],
    maxLength: scala.Option[scala.Int],
    name: gov.nasa.jpl.imce.oml.tables.LocalName,
    pattern: scala.Option[gov.nasa.jpl.imce.oml.tables.Pattern] )
  : (resolver.api.Extent, resolver.api.StringScalarRestriction)
  = {
    // factoryMethodWithUUIDGenerator
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val stringScalarRestriction = StringScalarRestriction( uuid, restrictedRange, length, minLength, maxLength, name, pattern )
    scala.Tuple2(
      extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, stringScalarRestriction),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (stringScalarRestriction -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> stringScalarRestriction)),
    	stringScalarRestriction)
  }
  		  
  // Structure
  override def createStructure
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    name: gov.nasa.jpl.imce.oml.tables.LocalName )
  : (resolver.api.Extent, resolver.api.Structure)
  = {
    // factoryMethodWithUUIDGenerator
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val structure = Structure( uuid, name )
    scala.Tuple2(
      extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, structure),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (structure -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> structure)),
    	structure)
  }
  		  
  // StructuredDataProperty
  override def createStructuredDataProperty
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    domain: resolver.api.Structure,
    range: resolver.api.Structure,
    name: gov.nasa.jpl.imce.oml.tables.LocalName )
  : (resolver.api.Extent, resolver.api.StructuredDataProperty)
  = {
    // factoryMethodWithUUIDGenerator
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val structuredDataProperty = StructuredDataProperty( uuid, domain, range, name )
    scala.Tuple2(
      extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, structuredDataProperty),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (structuredDataProperty -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> structuredDataProperty)),
    	structuredDataProperty)
  }
  		  
  // StructuredDataPropertyValue
  override def createStructuredDataPropertyValue
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    singletonInstance: resolver.api.SingletonInstance,
    structuredDataProperty: resolver.api.DataRelationshipToStructure )
  : (resolver.api.Extent, resolver.api.StructuredDataPropertyValue)
  = {
    // factoryMethodWithDerivedUUID
    // container: singletonInstance SingletonInstance
    // contained: structuredDataPropertyValues StructuredDataPropertyValue
    val structuredDataPropertyValue = StructuredDataPropertyValue( uuid, structuredDataProperty )
    scala.Tuple2(
    	extent.copy(
    	  structuredDataPropertyValues = extent.withStructuredDataPropertyValue(singletonInstance, structuredDataPropertyValue),
    	  singletonInstanceOfStructuredDataPropertyValue = extent.singletonInstanceOfStructuredDataPropertyValue + (structuredDataPropertyValue -> singletonInstance),
    	  structuredDataPropertyValueByUUID = extent.structuredDataPropertyValueByUUID + (uuid -> structuredDataPropertyValue)),
    	structuredDataPropertyValue)
  }
  		  
  // SynonymScalarRestriction
  override def createSynonymScalarRestriction
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    restrictedRange: resolver.api.DataRange,
    name: gov.nasa.jpl.imce.oml.tables.LocalName )
  : (resolver.api.Extent, resolver.api.SynonymScalarRestriction)
  = {
    // factoryMethodWithUUIDGenerator
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val synonymScalarRestriction = SynonymScalarRestriction( uuid, restrictedRange, name )
    scala.Tuple2(
      extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, synonymScalarRestriction),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (synonymScalarRestriction -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> synonymScalarRestriction)),
    	synonymScalarRestriction)
  }
  		  
  // TerminologyExtensionAxiom
  override def createTerminologyExtensionAxiom
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    extendedTerminology: resolver.api.TerminologyBox )
  : (resolver.api.Extent, resolver.api.TerminologyExtensionAxiom)
  = {
    // factoryMethodWithDerivedUUID
    // container: tbox TerminologyBox
    // contained: boxAxioms TerminologyBoxAxiom
    val terminologyExtensionAxiom = TerminologyExtensionAxiom( uuid, extendedTerminology )
    scala.Tuple2(
    	extent.copy(
    	  boxAxioms = extent.withTerminologyBoxAxiom(tbox, terminologyExtensionAxiom),
    	  terminologyBoxOfTerminologyBoxAxiom = extent.terminologyBoxOfTerminologyBoxAxiom + (terminologyExtensionAxiom -> tbox),
    	  terminologyBoxAxiomByUUID = extent.terminologyBoxAxiomByUUID + (uuid -> terminologyExtensionAxiom)),
    	terminologyExtensionAxiom)
  }
  		  
  // TerminologyGraph
  override def createTerminologyGraph
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    kind: gov.nasa.jpl.imce.oml.tables.TerminologyKind,
    iri: gov.nasa.jpl.imce.oml.tables.IRI )
  : (resolver.api.Extent, resolver.api.TerminologyGraph)
  = {
    val terminologyGraph = TerminologyGraph( uuid, kind, iri )
    scala.Tuple2(
  	extent.copy(terminologyGraphs = extent.terminologyGraphs + (uuid -> terminologyGraph)), 
   	terminologyGraph)
  }
  		  
  // TerminologyNestingAxiom
  override def createTerminologyNestingAxiom
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    nestingTerminology: resolver.api.TerminologyBox,
    nestingContext: resolver.api.Concept )
  : (resolver.api.Extent, resolver.api.TerminologyNestingAxiom)
  = {
    // factoryMethodWithDerivedUUID
    // container: tbox TerminologyBox
    // contained: boxAxioms TerminologyBoxAxiom
    val terminologyNestingAxiom = TerminologyNestingAxiom( uuid, nestingTerminology, nestingContext )
    scala.Tuple2(
    	extent.copy(
    	  boxAxioms = extent.withTerminologyBoxAxiom(tbox, terminologyNestingAxiom),
    	  terminologyBoxOfTerminologyBoxAxiom = extent.terminologyBoxOfTerminologyBoxAxiom + (terminologyNestingAxiom -> tbox),
    	  terminologyBoxAxiomByUUID = extent.terminologyBoxAxiomByUUID + (uuid -> terminologyNestingAxiom)),
    	terminologyNestingAxiom)
  }
  		  
  // TimeScalarRestriction
  override def createTimeScalarRestriction
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
    restrictedRange: resolver.api.DataRange,
    minExclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalTime],
    minInclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalTime],
    maxExclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalTime],
    maxInclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LexicalTime],
    name: gov.nasa.jpl.imce.oml.tables.LocalName )
  : (resolver.api.Extent, resolver.api.TimeScalarRestriction)
  = {
    // factoryMethodWithUUIDGenerator
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val timeScalarRestriction = TimeScalarRestriction( uuid, restrictedRange, minExclusive, minInclusive, maxExclusive, maxInclusive, name )
    scala.Tuple2(
      extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, timeScalarRestriction),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (timeScalarRestriction -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> timeScalarRestriction)),
    	timeScalarRestriction)
  }
  		  
  // UnreifiedRelationship
  override def createUnreifiedRelationship
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    tbox: resolver.api.TerminologyBox,
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
    name: gov.nasa.jpl.imce.oml.tables.LocalName )
  : (resolver.api.Extent, resolver.api.UnreifiedRelationship)
  = {
    // factoryMethodWithUUIDGenerator
    // container: tbox TerminologyBox
    // contained: boxStatements TerminologyBoxStatement
    val unreifiedRelationship = UnreifiedRelationship( uuid, source, target, isAsymmetric, isEssential, isFunctional, isInverseEssential, isInverseFunctional, isIrreflexive, isReflexive, isSymmetric, isTransitive, name )
    scala.Tuple2(
      extent.copy(
    	  boxStatements = extent.withTerminologyBoxStatement(tbox, unreifiedRelationship),
    	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (unreifiedRelationship -> tbox),
    	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> unreifiedRelationship)),
    	unreifiedRelationship)
  }
  		  
  // UnreifiedRelationshipInstanceTuple
  override def createUnreifiedRelationshipInstanceTuple
  ( extent: resolver.api.Extent,
    uuid: java.util.UUID,
    descriptionBox: resolver.api.DescriptionBox,
    unreifiedRelationship: resolver.api.UnreifiedRelationship,
    domain: resolver.api.ConceptualEntitySingletonInstance,
    range: resolver.api.ConceptualEntitySingletonInstance )
  : (resolver.api.Extent, resolver.api.UnreifiedRelationshipInstanceTuple)
  = {
    // factoryMethodWithDerivedUUID
    // container: descriptionBox DescriptionBox
    // contained: unreifiedRelationshipInstanceTuples UnreifiedRelationshipInstanceTuple
    val unreifiedRelationshipInstanceTuple = UnreifiedRelationshipInstanceTuple( uuid, unreifiedRelationship, domain, range )
    scala.Tuple2(
    	extent.copy(
    	  unreifiedRelationshipInstanceTuples = extent.withUnreifiedRelationshipInstanceTuple(descriptionBox, unreifiedRelationshipInstanceTuple),
    	  descriptionBoxOfUnreifiedRelationshipInstanceTuple = extent.descriptionBoxOfUnreifiedRelationshipInstanceTuple + (unreifiedRelationshipInstanceTuple -> descriptionBox),
    	  unreifiedRelationshipInstanceTupleByUUID = extent.unreifiedRelationshipInstanceTupleByUUID + (uuid -> unreifiedRelationshipInstanceTuple)),
    	unreifiedRelationshipInstanceTuple)
  }
  		  
}
