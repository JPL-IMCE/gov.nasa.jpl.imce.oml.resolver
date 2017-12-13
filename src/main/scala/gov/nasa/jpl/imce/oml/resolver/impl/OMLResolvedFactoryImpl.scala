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
	 
	 // AnnotationProperty
	 override def createAnnotationProperty
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.AnnotationPropertyUUID,
	   iri: gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI,
	   abbrevIRI: gov.nasa.jpl.imce.oml.tables.taggedTypes.AbbrevIRI )
	 : (resolver.api.Extent, resolver.api.AnnotationProperty)
	 = {
	   val annotationProperty = AnnotationProperty( uuid, iri, abbrevIRI )
	   scala.Tuple2(
	 	extent.copy(annotationProperties = extent.annotationProperties + (uuid -> annotationProperty)), 
	 		annotationProperty)
	 }
	 		  
	 // AnnotationPropertyValue
	 override def createAnnotationPropertyValue
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.AnnotationPropertyValueUUID,
	   subject: resolver.api.LogicalElement,
	   property: resolver.api.AnnotationProperty,
	   value: gov.nasa.jpl.imce.oml.tables.taggedTypes.StringDataType )
	 : (resolver.api.Extent, resolver.api.AnnotationPropertyValue)
	 = {
	   // factoryMethodWithImplicitlyDerivedUUID
	   // container: subject LogicalElement
	   // contained: annotations AnnotationPropertyValue
	   val annotationPropertyValue = AnnotationPropertyValue( uuid, subject, property, value )
	   scala.Tuple2(
	   	extent.copy(
	   	  annotations = extent.withAnnotationPropertyValue(subject, annotationPropertyValue),
	   	  logicalElementOfAnnotationPropertyValue = extent.logicalElementOfAnnotationPropertyValue + (annotationPropertyValue -> subject),
	   	  annotationPropertyValueByUUID = extent.annotationPropertyValueByUUID + (uuid -> annotationPropertyValue)),
	   	annotationPropertyValue)
	 }
	 		  
	 // AnonymousConceptUnionAxiom
	 override def createAnonymousConceptUnionAxiom
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.AnonymousConceptUnionAxiomUUID,
	   disjointTaxonomyParent: resolver.api.ConceptTreeDisjunction,
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName )
	 : (resolver.api.Extent, resolver.api.AnonymousConceptUnionAxiom)
	 = {
	   // factoryMethodWithUUIDGenerator
	   // container: disjointTaxonomyParent ConceptTreeDisjunction
	   // contained: disjunctions DisjointUnionOfConceptsAxiom
	   val anonymousConceptUnionAxiom = AnonymousConceptUnionAxiom( uuid, name )
	   scala.Tuple2(
	     extent.copy(
	      disjunctions = extent.withDisjointUnionOfConceptsAxiom(disjointTaxonomyParent, anonymousConceptUnionAxiom),
	      conceptTreeDisjunctionOfDisjointUnionOfConceptsAxiom = extent.conceptTreeDisjunctionOfDisjointUnionOfConceptsAxiom + (anonymousConceptUnionAxiom -> disjointTaxonomyParent),
	      disjointUnionOfConceptsAxiomByUUID = extent.disjointUnionOfConceptsAxiomByUUID + (uuid -> anonymousConceptUnionAxiom)),
	   	anonymousConceptUnionAxiom)
	 }
	 		  
	 // Aspect
	 override def createAspect
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.AspectUUID,
	   tbox: resolver.api.TerminologyBox,
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName )
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
	 		  
	 // AspectPredicate
	 override def createAspectPredicate
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.AspectPredicateUUID,
	   aspect: resolver.api.Aspect,
	   bodySegment: resolver.api.RuleBodySegment )
	 : (resolver.api.Extent, resolver.api.AspectPredicate)
	 = {
	   // factoryMethodWithImplicitlyDerivedUUID
	   // container: bodySegment RuleBodySegment
	   // contained: predicate SegmentPredicate
	   val aspectPredicate = AspectPredicate( uuid, aspect, bodySegment )
	   scala.Tuple2(
	   	extent.copy(
	   	  predicate = extent.withSegmentPredicate(bodySegment, aspectPredicate),
	   	  ruleBodySegmentOfSegmentPredicate = extent.ruleBodySegmentOfSegmentPredicate + (aspectPredicate -> bodySegment),
	   	  segmentPredicateByUUID = extent.segmentPredicateByUUID + (uuid -> aspectPredicate)),
	   	aspectPredicate)
	 }
	 		  
	 // AspectSpecializationAxiom
	 override def createAspectSpecializationAxiom
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.AspectSpecializationAxiomUUID,
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
	   uuid: resolver.api.taggedTypes.BinaryScalarRestrictionUUID,
	   tbox: resolver.api.TerminologyBox,
	   restrictedRange: resolver.api.DataRange,
	   length: scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.PositiveIntegerLiteral],
	   minLength: scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.PositiveIntegerLiteral],
	   maxLength: scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.PositiveIntegerLiteral],
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName )
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
	   uuid: resolver.api.taggedTypes.BundleUUID,
	   kind: gov.nasa.jpl.imce.oml.tables.TerminologyKind,
	   iri: gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI )
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
	   uuid: resolver.api.taggedTypes.BundledTerminologyAxiomUUID,
	   bundle: resolver.api.Bundle,
	   bundledTerminology: gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI )
	 : (resolver.api.Extent, resolver.api.BundledTerminologyAxiom)
	 = {
	   // factoryMethodWithDerivedUUID
	   // container: bundle Bundle
	   // contained: bundleAxioms TerminologyBundleAxiom
	   val bundledTerminologyAxiom = BundledTerminologyAxiom( uuid, bundle, bundledTerminology )
	   scala.Tuple2(
	   	extent.copy(
	   	  bundleAxioms = extent.withTerminologyBundleAxiom(bundle, bundledTerminologyAxiom),
	   	  bundleOfTerminologyBundleAxiom = extent.bundleOfTerminologyBundleAxiom + (bundledTerminologyAxiom -> bundle),
	   	  terminologyBundleAxiomByUUID = extent.terminologyBundleAxiomByUUID + (uuid -> bundledTerminologyAxiom)),
	   	bundledTerminologyAxiom)
	 }
	 		  
	 // ChainRule
	 override def createChainRule
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.ChainRuleUUID,
	   tbox: resolver.api.TerminologyBox,
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName,
	   head: resolver.api.UnreifiedRelationship )
	 : (resolver.api.Extent, resolver.api.ChainRule)
	 = {
	   // factoryMethodWithUUIDGenerator
	   // container: tbox TerminologyBox
	   // contained: boxStatements TerminologyBoxStatement
	   val chainRule = ChainRule( uuid, name, head )
	   scala.Tuple2(
	     extent.copy(
	      boxStatements = extent.withTerminologyBoxStatement(tbox, chainRule),
	      terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (chainRule -> tbox),
	      terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> chainRule)),
	   	chainRule)
	 }
	 		  
	 // Concept
	 override def createConcept
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.ConceptUUID,
	   tbox: resolver.api.TerminologyBox,
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName )
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
	   uuid: resolver.api.taggedTypes.ConceptDesignationTerminologyAxiomUUID,
	   tbox: resolver.api.TerminologyBox,
	   designatedConcept: resolver.api.Concept,
	   designatedTerminology: gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI )
	 : (resolver.api.Extent, resolver.api.ConceptDesignationTerminologyAxiom)
	 = {
	   // factoryMethodWithDerivedUUID
	   // container: tbox TerminologyBox
	   // contained: boxAxioms TerminologyBoxAxiom
	   val conceptDesignationTerminologyAxiom = ConceptDesignationTerminologyAxiom( uuid, tbox, designatedConcept, designatedTerminology )
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
	   uuid: resolver.api.taggedTypes.ConceptInstanceUUID,
	   descriptionBox: resolver.api.DescriptionBox,
	   singletonConceptClassifier: resolver.api.Concept,
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName )
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
	 		  
	 // ConceptPredicate
	 override def createConceptPredicate
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.ConceptPredicateUUID,
	   bodySegment: resolver.api.RuleBodySegment,
	   concept: resolver.api.Concept )
	 : (resolver.api.Extent, resolver.api.ConceptPredicate)
	 = {
	   // factoryMethodWithImplicitlyDerivedUUID
	   // container: bodySegment RuleBodySegment
	   // contained: predicate SegmentPredicate
	   val conceptPredicate = ConceptPredicate( uuid, bodySegment, concept )
	   scala.Tuple2(
	   	extent.copy(
	   	  predicate = extent.withSegmentPredicate(bodySegment, conceptPredicate),
	   	  ruleBodySegmentOfSegmentPredicate = extent.ruleBodySegmentOfSegmentPredicate + (conceptPredicate -> bodySegment),
	   	  segmentPredicateByUUID = extent.segmentPredicateByUUID + (uuid -> conceptPredicate)),
	   	conceptPredicate)
	 }
	 		  
	 // ConceptSpecializationAxiom
	 override def createConceptSpecializationAxiom
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.ConceptSpecializationAxiomUUID,
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
	 		  
	 // DescriptionBox
	 override def createDescriptionBox
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.DescriptionBoxUUID,
	   kind: gov.nasa.jpl.imce.oml.tables.DescriptionKind,
	   iri: gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI )
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
	   uuid: resolver.api.taggedTypes.DescriptionBoxExtendsClosedWorldDefinitionsUUID,
	   descriptionBox: resolver.api.DescriptionBox,
	   closedWorldDefinitions: gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI )
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
	   uuid: resolver.api.taggedTypes.DescriptionBoxRefinementUUID,
	   refiningDescriptionBox: resolver.api.DescriptionBox,
	   refinedDescriptionBox: gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI )
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
	   uuid: resolver.api.taggedTypes.EntityExistentialRestrictionAxiomUUID,
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
	   uuid: resolver.api.taggedTypes.EntityScalarDataPropertyUUID,
	   tbox: resolver.api.TerminologyBox,
	   domain: resolver.api.Entity,
	   range: resolver.api.DataRange,
	   isIdentityCriteria: scala.Boolean,
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName )
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
	   uuid: resolver.api.taggedTypes.EntityScalarDataPropertyExistentialRestrictionAxiomUUID,
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
	   uuid: resolver.api.taggedTypes.EntityScalarDataPropertyParticularRestrictionAxiomUUID,
	   tbox: resolver.api.TerminologyBox,
	   restrictedEntity: resolver.api.Entity,
	   scalarProperty: resolver.api.EntityScalarDataProperty,
	   literalValue: gov.nasa.jpl.imce.oml.tables.LiteralValue,
	   valueType: scala.Option[resolver.api.DataRange] )
	 : (resolver.api.Extent, resolver.api.EntityScalarDataPropertyParticularRestrictionAxiom)
	 = {
	   // factoryMethodWithDerivedUUID
	   // container: tbox TerminologyBox
	   // contained: boxStatements TerminologyBoxStatement
	   val entityScalarDataPropertyParticularRestrictionAxiom = EntityScalarDataPropertyParticularRestrictionAxiom( uuid, restrictedEntity, scalarProperty, literalValue, valueType )
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
	   uuid: resolver.api.taggedTypes.EntityScalarDataPropertyUniversalRestrictionAxiomUUID,
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
	   uuid: resolver.api.taggedTypes.EntityStructuredDataPropertyUUID,
	   tbox: resolver.api.TerminologyBox,
	   domain: resolver.api.Entity,
	   range: resolver.api.Structure,
	   isIdentityCriteria: scala.Boolean,
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName )
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
	 		  
	 // EntityStructuredDataPropertyParticularRestrictionAxiom
	 override def createEntityStructuredDataPropertyParticularRestrictionAxiom
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.EntityStructuredDataPropertyParticularRestrictionAxiomUUID,
	   tbox: resolver.api.TerminologyBox,
	   structuredDataProperty: resolver.api.DataRelationshipToStructure,
	   restrictedEntity: resolver.api.Entity )
	 : (resolver.api.Extent, resolver.api.EntityStructuredDataPropertyParticularRestrictionAxiom)
	 = {
	   // factoryMethodWithDerivedUUID
	   // container: tbox TerminologyBox
	   // contained: boxStatements TerminologyBoxStatement
	   val entityStructuredDataPropertyParticularRestrictionAxiom = EntityStructuredDataPropertyParticularRestrictionAxiom( uuid, structuredDataProperty, restrictedEntity )
	   scala.Tuple2(
	   	extent.copy(
	   	  boxStatements = extent.withTerminologyBoxStatement(tbox, entityStructuredDataPropertyParticularRestrictionAxiom),
	   	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (entityStructuredDataPropertyParticularRestrictionAxiom -> tbox),
	   	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> entityStructuredDataPropertyParticularRestrictionAxiom)),
	   	entityStructuredDataPropertyParticularRestrictionAxiom)
	 }
	 		  
	 // EntityUniversalRestrictionAxiom
	 override def createEntityUniversalRestrictionAxiom
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.EntityUniversalRestrictionAxiomUUID,
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
	   uuid: resolver.api.taggedTypes.IRIScalarRestrictionUUID,
	   tbox: resolver.api.TerminologyBox,
	   restrictedRange: resolver.api.DataRange,
	   length: scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.PositiveIntegerLiteral],
	   minLength: scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.PositiveIntegerLiteral],
	   maxLength: scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.PositiveIntegerLiteral],
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName,
	   pattern: scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.LiteralPattern] )
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
	   uuid: resolver.api.taggedTypes.NumericScalarRestrictionUUID,
	   tbox: resolver.api.TerminologyBox,
	   restrictedRange: resolver.api.DataRange,
	   minExclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LiteralNumber],
	   minInclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LiteralNumber],
	   maxExclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LiteralNumber],
	   maxInclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LiteralNumber],
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName )
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
	   uuid: resolver.api.taggedTypes.PlainLiteralScalarRestrictionUUID,
	   tbox: resolver.api.TerminologyBox,
	   restrictedRange: resolver.api.DataRange,
	   length: scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.PositiveIntegerLiteral],
	   minLength: scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.PositiveIntegerLiteral],
	   maxLength: scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.PositiveIntegerLiteral],
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName,
	   langRange: scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.LanguageTagDataType],
	   pattern: scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.LiteralPattern] )
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
	   uuid: resolver.api.taggedTypes.ReifiedRelationshipUUID,
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
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName,
	   unreifiedPropertyName: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName,
	   unreifiedInversePropertyName: scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName] )
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
	   uuid: resolver.api.taggedTypes.ReifiedRelationshipInstanceUUID,
	   descriptionBox: resolver.api.DescriptionBox,
	   singletonReifiedRelationshipClassifier: resolver.api.ReifiedRelationship,
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName )
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
	   uuid: resolver.api.taggedTypes.ReifiedRelationshipInstanceDomainUUID,
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
	   uuid: resolver.api.taggedTypes.ReifiedRelationshipInstanceRangeUUID,
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
	 		  
	 // ReifiedRelationshipInversePropertyPredicate
	 override def createReifiedRelationshipInversePropertyPredicate
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.ReifiedRelationshipInversePropertyPredicateUUID,
	   bodySegment: resolver.api.RuleBodySegment,
	   reifiedRelationship: resolver.api.ReifiedRelationship )
	 : (resolver.api.Extent, resolver.api.ReifiedRelationshipInversePropertyPredicate)
	 = {
	   // factoryMethodWithImplicitlyDerivedUUID
	   // container: bodySegment RuleBodySegment
	   // contained: predicate SegmentPredicate
	   val reifiedRelationshipInversePropertyPredicate = ReifiedRelationshipInversePropertyPredicate( uuid, bodySegment, reifiedRelationship )
	   scala.Tuple2(
	   	extent.copy(
	   	  predicate = extent.withSegmentPredicate(bodySegment, reifiedRelationshipInversePropertyPredicate),
	   	  ruleBodySegmentOfSegmentPredicate = extent.ruleBodySegmentOfSegmentPredicate + (reifiedRelationshipInversePropertyPredicate -> bodySegment),
	   	  segmentPredicateByUUID = extent.segmentPredicateByUUID + (uuid -> reifiedRelationshipInversePropertyPredicate)),
	   	reifiedRelationshipInversePropertyPredicate)
	 }
	 		  
	 // ReifiedRelationshipPredicate
	 override def createReifiedRelationshipPredicate
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.ReifiedRelationshipPredicateUUID,
	   bodySegment: resolver.api.RuleBodySegment,
	   reifiedRelationship: resolver.api.ReifiedRelationship )
	 : (resolver.api.Extent, resolver.api.ReifiedRelationshipPredicate)
	 = {
	   // factoryMethodWithImplicitlyDerivedUUID
	   // container: bodySegment RuleBodySegment
	   // contained: predicate SegmentPredicate
	   val reifiedRelationshipPredicate = ReifiedRelationshipPredicate( uuid, bodySegment, reifiedRelationship )
	   scala.Tuple2(
	   	extent.copy(
	   	  predicate = extent.withSegmentPredicate(bodySegment, reifiedRelationshipPredicate),
	   	  ruleBodySegmentOfSegmentPredicate = extent.ruleBodySegmentOfSegmentPredicate + (reifiedRelationshipPredicate -> bodySegment),
	   	  segmentPredicateByUUID = extent.segmentPredicateByUUID + (uuid -> reifiedRelationshipPredicate)),
	   	reifiedRelationshipPredicate)
	 }
	 		  
	 // ReifiedRelationshipPropertyPredicate
	 override def createReifiedRelationshipPropertyPredicate
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.ReifiedRelationshipPropertyPredicateUUID,
	   bodySegment: resolver.api.RuleBodySegment,
	   reifiedRelationship: resolver.api.ReifiedRelationship )
	 : (resolver.api.Extent, resolver.api.ReifiedRelationshipPropertyPredicate)
	 = {
	   // factoryMethodWithImplicitlyDerivedUUID
	   // container: bodySegment RuleBodySegment
	   // contained: predicate SegmentPredicate
	   val reifiedRelationshipPropertyPredicate = ReifiedRelationshipPropertyPredicate( uuid, bodySegment, reifiedRelationship )
	   scala.Tuple2(
	   	extent.copy(
	   	  predicate = extent.withSegmentPredicate(bodySegment, reifiedRelationshipPropertyPredicate),
	   	  ruleBodySegmentOfSegmentPredicate = extent.ruleBodySegmentOfSegmentPredicate + (reifiedRelationshipPropertyPredicate -> bodySegment),
	   	  segmentPredicateByUUID = extent.segmentPredicateByUUID + (uuid -> reifiedRelationshipPropertyPredicate)),
	   	reifiedRelationshipPropertyPredicate)
	 }
	 		  
	 // ReifiedRelationshipSourceInversePropertyPredicate
	 override def createReifiedRelationshipSourceInversePropertyPredicate
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.ReifiedRelationshipSourceInversePropertyPredicateUUID,
	   bodySegment: resolver.api.RuleBodySegment,
	   reifiedRelationship: resolver.api.ReifiedRelationship )
	 : (resolver.api.Extent, resolver.api.ReifiedRelationshipSourceInversePropertyPredicate)
	 = {
	   // factoryMethodWithImplicitlyDerivedUUID
	   // container: bodySegment RuleBodySegment
	   // contained: predicate SegmentPredicate
	   val reifiedRelationshipSourceInversePropertyPredicate = ReifiedRelationshipSourceInversePropertyPredicate( uuid, bodySegment, reifiedRelationship )
	   scala.Tuple2(
	   	extent.copy(
	   	  predicate = extent.withSegmentPredicate(bodySegment, reifiedRelationshipSourceInversePropertyPredicate),
	   	  ruleBodySegmentOfSegmentPredicate = extent.ruleBodySegmentOfSegmentPredicate + (reifiedRelationshipSourceInversePropertyPredicate -> bodySegment),
	   	  segmentPredicateByUUID = extent.segmentPredicateByUUID + (uuid -> reifiedRelationshipSourceInversePropertyPredicate)),
	   	reifiedRelationshipSourceInversePropertyPredicate)
	 }
	 		  
	 // ReifiedRelationshipSourcePropertyPredicate
	 override def createReifiedRelationshipSourcePropertyPredicate
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.ReifiedRelationshipSourcePropertyPredicateUUID,
	   bodySegment: resolver.api.RuleBodySegment,
	   reifiedRelationship: resolver.api.ReifiedRelationship )
	 : (resolver.api.Extent, resolver.api.ReifiedRelationshipSourcePropertyPredicate)
	 = {
	   // factoryMethodWithImplicitlyDerivedUUID
	   // container: bodySegment RuleBodySegment
	   // contained: predicate SegmentPredicate
	   val reifiedRelationshipSourcePropertyPredicate = ReifiedRelationshipSourcePropertyPredicate( uuid, bodySegment, reifiedRelationship )
	   scala.Tuple2(
	   	extent.copy(
	   	  predicate = extent.withSegmentPredicate(bodySegment, reifiedRelationshipSourcePropertyPredicate),
	   	  ruleBodySegmentOfSegmentPredicate = extent.ruleBodySegmentOfSegmentPredicate + (reifiedRelationshipSourcePropertyPredicate -> bodySegment),
	   	  segmentPredicateByUUID = extent.segmentPredicateByUUID + (uuid -> reifiedRelationshipSourcePropertyPredicate)),
	   	reifiedRelationshipSourcePropertyPredicate)
	 }
	 		  
	 // ReifiedRelationshipSpecializationAxiom
	 override def createReifiedRelationshipSpecializationAxiom
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.ReifiedRelationshipSpecializationAxiomUUID,
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
	 		  
	 // ReifiedRelationshipTargetInversePropertyPredicate
	 override def createReifiedRelationshipTargetInversePropertyPredicate
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.ReifiedRelationshipTargetInversePropertyPredicateUUID,
	   bodySegment: resolver.api.RuleBodySegment,
	   reifiedRelationship: resolver.api.ReifiedRelationship )
	 : (resolver.api.Extent, resolver.api.ReifiedRelationshipTargetInversePropertyPredicate)
	 = {
	   // factoryMethodWithImplicitlyDerivedUUID
	   // container: bodySegment RuleBodySegment
	   // contained: predicate SegmentPredicate
	   val reifiedRelationshipTargetInversePropertyPredicate = ReifiedRelationshipTargetInversePropertyPredicate( uuid, bodySegment, reifiedRelationship )
	   scala.Tuple2(
	   	extent.copy(
	   	  predicate = extent.withSegmentPredicate(bodySegment, reifiedRelationshipTargetInversePropertyPredicate),
	   	  ruleBodySegmentOfSegmentPredicate = extent.ruleBodySegmentOfSegmentPredicate + (reifiedRelationshipTargetInversePropertyPredicate -> bodySegment),
	   	  segmentPredicateByUUID = extent.segmentPredicateByUUID + (uuid -> reifiedRelationshipTargetInversePropertyPredicate)),
	   	reifiedRelationshipTargetInversePropertyPredicate)
	 }
	 		  
	 // ReifiedRelationshipTargetPropertyPredicate
	 override def createReifiedRelationshipTargetPropertyPredicate
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.ReifiedRelationshipTargetPropertyPredicateUUID,
	   bodySegment: resolver.api.RuleBodySegment,
	   reifiedRelationship: resolver.api.ReifiedRelationship )
	 : (resolver.api.Extent, resolver.api.ReifiedRelationshipTargetPropertyPredicate)
	 = {
	   // factoryMethodWithImplicitlyDerivedUUID
	   // container: bodySegment RuleBodySegment
	   // contained: predicate SegmentPredicate
	   val reifiedRelationshipTargetPropertyPredicate = ReifiedRelationshipTargetPropertyPredicate( uuid, bodySegment, reifiedRelationship )
	   scala.Tuple2(
	   	extent.copy(
	   	  predicate = extent.withSegmentPredicate(bodySegment, reifiedRelationshipTargetPropertyPredicate),
	   	  ruleBodySegmentOfSegmentPredicate = extent.ruleBodySegmentOfSegmentPredicate + (reifiedRelationshipTargetPropertyPredicate -> bodySegment),
	   	  segmentPredicateByUUID = extent.segmentPredicateByUUID + (uuid -> reifiedRelationshipTargetPropertyPredicate)),
	   	reifiedRelationshipTargetPropertyPredicate)
	 }
	 		  
	 // RestrictionScalarDataPropertyValue
	 override def createRestrictionScalarDataPropertyValue
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.RestrictionScalarDataPropertyValueUUID,
	   scalarDataProperty: resolver.api.DataRelationshipToScalar,
	   scalarPropertyValue: gov.nasa.jpl.imce.oml.tables.LiteralValue,
	   structuredDataPropertyContext: resolver.api.RestrictionStructuredDataPropertyContext,
	   valueType: scala.Option[resolver.api.DataRange] )
	 : (resolver.api.Extent, resolver.api.RestrictionScalarDataPropertyValue)
	 = {
	   // factoryMethodWithDerivedUUID
	   // container: structuredDataPropertyContext RestrictionStructuredDataPropertyContext
	   // contained: scalarDataPropertyRestrictions RestrictionScalarDataPropertyValue
	   val restrictionScalarDataPropertyValue = RestrictionScalarDataPropertyValue( uuid, scalarDataProperty, scalarPropertyValue, valueType )
	   scala.Tuple2(
	   	extent.copy(
	   	  scalarDataPropertyRestrictions = extent.withRestrictionScalarDataPropertyValue(structuredDataPropertyContext, restrictionScalarDataPropertyValue),
	   	  restrictionStructuredDataPropertyContextOfRestrictionScalarDataPropertyValue = extent.restrictionStructuredDataPropertyContextOfRestrictionScalarDataPropertyValue + (restrictionScalarDataPropertyValue -> structuredDataPropertyContext),
	   	  restrictionScalarDataPropertyValueByUUID = extent.restrictionScalarDataPropertyValueByUUID + (uuid -> restrictionScalarDataPropertyValue)),
	   	restrictionScalarDataPropertyValue)
	 }
	 		  
	 // RestrictionStructuredDataPropertyTuple
	 override def createRestrictionStructuredDataPropertyTuple
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.RestrictionStructuredDataPropertyTupleUUID,
	   structuredDataProperty: resolver.api.DataRelationshipToStructure,
	   structuredDataPropertyContext: resolver.api.RestrictionStructuredDataPropertyContext )
	 : (resolver.api.Extent, resolver.api.RestrictionStructuredDataPropertyTuple)
	 = {
	   // factoryMethodWithDerivedUUID
	   // container: structuredDataPropertyContext RestrictionStructuredDataPropertyContext
	   // contained: structuredDataPropertyRestrictions RestrictionStructuredDataPropertyTuple
	   val restrictionStructuredDataPropertyTuple = RestrictionStructuredDataPropertyTuple( uuid, structuredDataProperty )
	   scala.Tuple2(
	   	extent.copy(
	   	  structuredDataPropertyRestrictions = extent.withRestrictionStructuredDataPropertyTuple(structuredDataPropertyContext, restrictionStructuredDataPropertyTuple),
	   	  restrictionStructuredDataPropertyContextOfRestrictionStructuredDataPropertyTuple = extent.restrictionStructuredDataPropertyContextOfRestrictionStructuredDataPropertyTuple + (restrictionStructuredDataPropertyTuple -> structuredDataPropertyContext),
	   	  restrictionStructuredDataPropertyTupleByUUID = extent.restrictionStructuredDataPropertyTupleByUUID + (uuid -> restrictionStructuredDataPropertyTuple)),
	   	restrictionStructuredDataPropertyTuple)
	 }
	 		  
	 // RootConceptTaxonomyAxiom
	 override def createRootConceptTaxonomyAxiom
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.RootConceptTaxonomyAxiomUUID,
	   bundle: resolver.api.Bundle,
	   root: resolver.api.Concept )
	 : (resolver.api.Extent, resolver.api.RootConceptTaxonomyAxiom)
	 = {
	   // factoryMethodWithDerivedUUID
	   // container: bundle Bundle
	   // contained: bundleStatements TerminologyBundleStatement
	   val rootConceptTaxonomyAxiom = RootConceptTaxonomyAxiom( uuid, bundle, root )
	   scala.Tuple2(
	   	extent.copy(
	   	  bundleStatements = extent.withTerminologyBundleStatement(bundle, rootConceptTaxonomyAxiom),
	   	  bundleOfTerminologyBundleStatement = extent.bundleOfTerminologyBundleStatement + (rootConceptTaxonomyAxiom -> bundle),
	   	  terminologyBundleStatementByUUID = extent.terminologyBundleStatementByUUID + (uuid -> rootConceptTaxonomyAxiom)),
	   	rootConceptTaxonomyAxiom)
	 }
	 		  
	 // RuleBodySegment
	 override def createRuleBodySegment
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.RuleBodySegmentUUID,
	   previousSegment: scala.Option[resolver.api.RuleBodySegment],
	   rule: scala.Option[resolver.api.ChainRule] )
	 : (resolver.api.Extent, resolver.api.RuleBodySegment)
	 = {
	   // factoryMethodWithUUIDGenerator (scala...)
	   // container: previousSegment RuleBodySegment
	   // contained: nextSegment RuleBodySegment
	   val ruleBodySegment = RuleBodySegment( uuid, previousSegment, rule )
	   scala.Tuple2(
	     previousSegment.fold {
	     extent.copy(
	      ruleBodySegmentByUUID = extent.ruleBodySegmentByUUID + (uuid -> ruleBodySegment))
	     }{ _previousSegment_ =>
	     extent.copy(
	      nextSegment = extent.withRuleBodySegment(_previousSegment_, ruleBodySegment),
	      ruleBodySegmentOfRuleBodySegment = extent.ruleBodySegmentOfRuleBodySegment + (ruleBodySegment -> _previousSegment_),
	      ruleBodySegmentByUUID = extent.ruleBodySegmentByUUID + (uuid -> ruleBodySegment))
	     },
	   	ruleBodySegment)
	 }
	 		  
	 // Scalar
	 override def createScalar
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.ScalarUUID,
	   tbox: resolver.api.TerminologyBox,
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName )
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
	   uuid: resolver.api.taggedTypes.ScalarDataPropertyUUID,
	   tbox: resolver.api.TerminologyBox,
	   domain: resolver.api.Structure,
	   range: resolver.api.DataRange,
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName )
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
	   uuid: resolver.api.taggedTypes.ScalarDataPropertyValueUUID,
	   scalarDataProperty: resolver.api.DataRelationshipToScalar,
	   scalarPropertyValue: gov.nasa.jpl.imce.oml.tables.LiteralValue,
	   structuredDataPropertyContext: resolver.api.SingletonInstanceStructuredDataPropertyContext,
	   valueType: scala.Option[resolver.api.DataRange] )
	 : (resolver.api.Extent, resolver.api.ScalarDataPropertyValue)
	 = {
	   // factoryMethodWithDerivedUUID
	   // container: structuredDataPropertyContext SingletonInstanceStructuredDataPropertyContext
	   // contained: scalarDataPropertyValues ScalarDataPropertyValue
	   val scalarDataPropertyValue = ScalarDataPropertyValue( uuid, scalarDataProperty, scalarPropertyValue, valueType )
	   scala.Tuple2(
	   	extent.copy(
	   	  scalarDataPropertyValues = extent.withScalarDataPropertyValue(structuredDataPropertyContext, scalarDataPropertyValue),
	   	  singletonInstanceStructuredDataPropertyContextOfScalarDataPropertyValue = extent.singletonInstanceStructuredDataPropertyContextOfScalarDataPropertyValue + (scalarDataPropertyValue -> structuredDataPropertyContext),
	   	  scalarDataPropertyValueByUUID = extent.scalarDataPropertyValueByUUID + (uuid -> scalarDataPropertyValue)),
	   	scalarDataPropertyValue)
	 }
	 		  
	 // ScalarOneOfLiteralAxiom
	 override def createScalarOneOfLiteralAxiom
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.ScalarOneOfLiteralAxiomUUID,
	   tbox: resolver.api.TerminologyBox,
	   axiom: resolver.api.ScalarOneOfRestriction,
	   value: gov.nasa.jpl.imce.oml.tables.LiteralValue,
	   valueType: scala.Option[resolver.api.DataRange] )
	 : (resolver.api.Extent, resolver.api.ScalarOneOfLiteralAxiom)
	 = {
	   // factoryMethodWithDerivedUUID
	   // container: tbox TerminologyBox
	   // contained: boxStatements TerminologyBoxStatement
	   val scalarOneOfLiteralAxiom = ScalarOneOfLiteralAxiom( uuid, axiom, value, valueType )
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
	   uuid: resolver.api.taggedTypes.ScalarOneOfRestrictionUUID,
	   tbox: resolver.api.TerminologyBox,
	   restrictedRange: resolver.api.DataRange,
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName )
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
	 		  
	 // SingletonInstanceScalarDataPropertyValue
	 override def createSingletonInstanceScalarDataPropertyValue
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.SingletonInstanceScalarDataPropertyValueUUID,
	   descriptionBox: resolver.api.DescriptionBox,
	   singletonInstance: resolver.api.ConceptualEntitySingletonInstance,
	   scalarDataProperty: resolver.api.EntityScalarDataProperty,
	   scalarPropertyValue: gov.nasa.jpl.imce.oml.tables.LiteralValue,
	   valueType: scala.Option[resolver.api.DataRange] )
	 : (resolver.api.Extent, resolver.api.SingletonInstanceScalarDataPropertyValue)
	 = {
	   // factoryMethodWithDerivedUUID
	   // container: descriptionBox DescriptionBox
	   // contained: singletonScalarDataPropertyValues SingletonInstanceScalarDataPropertyValue
	   val singletonInstanceScalarDataPropertyValue = SingletonInstanceScalarDataPropertyValue( uuid, singletonInstance, scalarDataProperty, scalarPropertyValue, valueType )
	   scala.Tuple2(
	   	extent.copy(
	   	  singletonScalarDataPropertyValues = extent.withSingletonInstanceScalarDataPropertyValue(descriptionBox, singletonInstanceScalarDataPropertyValue),
	   	  descriptionBoxOfSingletonInstanceScalarDataPropertyValue = extent.descriptionBoxOfSingletonInstanceScalarDataPropertyValue + (singletonInstanceScalarDataPropertyValue -> descriptionBox),
	   	  singletonInstanceScalarDataPropertyValueByUUID = extent.singletonInstanceScalarDataPropertyValueByUUID + (uuid -> singletonInstanceScalarDataPropertyValue)),
	   	singletonInstanceScalarDataPropertyValue)
	 }
	 		  
	 // SingletonInstanceStructuredDataPropertyValue
	 override def createSingletonInstanceStructuredDataPropertyValue
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.SingletonInstanceStructuredDataPropertyValueUUID,
	   descriptionBox: resolver.api.DescriptionBox,
	   singletonInstance: resolver.api.ConceptualEntitySingletonInstance,
	   structuredDataProperty: resolver.api.DataRelationshipToStructure )
	 : (resolver.api.Extent, resolver.api.SingletonInstanceStructuredDataPropertyValue)
	 = {
	   // factoryMethodWithDerivedUUID
	   // container: descriptionBox DescriptionBox
	   // contained: singletonStructuredDataPropertyValues SingletonInstanceStructuredDataPropertyValue
	   val singletonInstanceStructuredDataPropertyValue = SingletonInstanceStructuredDataPropertyValue( uuid, singletonInstance, structuredDataProperty )
	   scala.Tuple2(
	   	extent.copy(
	   	  singletonStructuredDataPropertyValues = extent.withSingletonInstanceStructuredDataPropertyValue(descriptionBox, singletonInstanceStructuredDataPropertyValue),
	   	  descriptionBoxOfSingletonInstanceStructuredDataPropertyValue = extent.descriptionBoxOfSingletonInstanceStructuredDataPropertyValue + (singletonInstanceStructuredDataPropertyValue -> descriptionBox),
	   	  singletonInstanceStructuredDataPropertyValueByUUID = extent.singletonInstanceStructuredDataPropertyValueByUUID + (uuid -> singletonInstanceStructuredDataPropertyValue)),
	   	singletonInstanceStructuredDataPropertyValue)
	 }
	 		  
	 // SpecificDisjointConceptAxiom
	 override def createSpecificDisjointConceptAxiom
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.SpecificDisjointConceptAxiomUUID,
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
	   uuid: resolver.api.taggedTypes.StringScalarRestrictionUUID,
	   tbox: resolver.api.TerminologyBox,
	   restrictedRange: resolver.api.DataRange,
	   length: scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.PositiveIntegerLiteral],
	   minLength: scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.PositiveIntegerLiteral],
	   maxLength: scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.PositiveIntegerLiteral],
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName,
	   pattern: scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.LiteralPattern] )
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
	   uuid: resolver.api.taggedTypes.StructureUUID,
	   tbox: resolver.api.TerminologyBox,
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName )
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
	   uuid: resolver.api.taggedTypes.StructuredDataPropertyUUID,
	   tbox: resolver.api.TerminologyBox,
	   domain: resolver.api.Structure,
	   range: resolver.api.Structure,
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName )
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
	 		  
	 // StructuredDataPropertyTuple
	 override def createStructuredDataPropertyTuple
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.StructuredDataPropertyTupleUUID,
	   structuredDataProperty: resolver.api.DataRelationshipToStructure,
	   structuredDataPropertyContext: resolver.api.SingletonInstanceStructuredDataPropertyContext )
	 : (resolver.api.Extent, resolver.api.StructuredDataPropertyTuple)
	 = {
	   // factoryMethodWithDerivedUUID
	   // container: structuredDataPropertyContext SingletonInstanceStructuredDataPropertyContext
	   // contained: structuredPropertyTuples StructuredDataPropertyTuple
	   val structuredDataPropertyTuple = StructuredDataPropertyTuple( uuid, structuredDataProperty )
	   scala.Tuple2(
	   	extent.copy(
	   	  structuredPropertyTuples = extent.withStructuredDataPropertyTuple(structuredDataPropertyContext, structuredDataPropertyTuple),
	   	  singletonInstanceStructuredDataPropertyContextOfStructuredDataPropertyTuple = extent.singletonInstanceStructuredDataPropertyContextOfStructuredDataPropertyTuple + (structuredDataPropertyTuple -> structuredDataPropertyContext),
	   	  structuredDataPropertyTupleByUUID = extent.structuredDataPropertyTupleByUUID + (uuid -> structuredDataPropertyTuple)),
	   	structuredDataPropertyTuple)
	 }
	 		  
	 // SubDataPropertyOfAxiom
	 override def createSubDataPropertyOfAxiom
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.SubDataPropertyOfAxiomUUID,
	   tbox: resolver.api.TerminologyBox,
	   subProperty: resolver.api.EntityScalarDataProperty,
	   superProperty: resolver.api.EntityScalarDataProperty )
	 : (resolver.api.Extent, resolver.api.SubDataPropertyOfAxiom)
	 = {
	   // factoryMethodWithImplicitlyDerivedUUID
	   // container: tbox TerminologyBox
	   // contained: boxStatements TerminologyBoxStatement
	   val subDataPropertyOfAxiom = SubDataPropertyOfAxiom( uuid, subProperty, superProperty )
	   scala.Tuple2(
	   	extent.copy(
	   	  boxStatements = extent.withTerminologyBoxStatement(tbox, subDataPropertyOfAxiom),
	   	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (subDataPropertyOfAxiom -> tbox),
	   	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> subDataPropertyOfAxiom)),
	   	subDataPropertyOfAxiom)
	 }
	 		  
	 // SubObjectPropertyOfAxiom
	 override def createSubObjectPropertyOfAxiom
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.SubObjectPropertyOfAxiomUUID,
	   tbox: resolver.api.TerminologyBox,
	   subProperty: resolver.api.UnreifiedRelationship,
	   superProperty: resolver.api.UnreifiedRelationship )
	 : (resolver.api.Extent, resolver.api.SubObjectPropertyOfAxiom)
	 = {
	   // factoryMethodWithImplicitlyDerivedUUID
	   // container: tbox TerminologyBox
	   // contained: boxStatements TerminologyBoxStatement
	   val subObjectPropertyOfAxiom = SubObjectPropertyOfAxiom( uuid, subProperty, superProperty )
	   scala.Tuple2(
	   	extent.copy(
	   	  boxStatements = extent.withTerminologyBoxStatement(tbox, subObjectPropertyOfAxiom),
	   	  terminologyBoxOfTerminologyBoxStatement = extent.terminologyBoxOfTerminologyBoxStatement + (subObjectPropertyOfAxiom -> tbox),
	   	  terminologyBoxStatementByUUID = extent.terminologyBoxStatementByUUID + (uuid -> subObjectPropertyOfAxiom)),
	   	subObjectPropertyOfAxiom)
	 }
	 		  
	 // SynonymScalarRestriction
	 override def createSynonymScalarRestriction
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.SynonymScalarRestrictionUUID,
	   tbox: resolver.api.TerminologyBox,
	   restrictedRange: resolver.api.DataRange,
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName )
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
	   uuid: resolver.api.taggedTypes.TerminologyExtensionAxiomUUID,
	   tbox: resolver.api.TerminologyBox,
	   extendedTerminology: gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI )
	 : (resolver.api.Extent, resolver.api.TerminologyExtensionAxiom)
	 = {
	   // factoryMethodWithDerivedUUID
	   // container: tbox TerminologyBox
	   // contained: boxAxioms TerminologyBoxAxiom
	   val terminologyExtensionAxiom = TerminologyExtensionAxiom( uuid, tbox, extendedTerminology )
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
	   uuid: resolver.api.taggedTypes.TerminologyGraphUUID,
	   kind: gov.nasa.jpl.imce.oml.tables.TerminologyKind,
	   iri: gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI )
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
	   uuid: resolver.api.taggedTypes.TerminologyNestingAxiomUUID,
	   tbox: resolver.api.TerminologyBox,
	   nestingContext: resolver.api.Concept,
	   nestingTerminology: gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI )
	 : (resolver.api.Extent, resolver.api.TerminologyNestingAxiom)
	 = {
	   // factoryMethodWithDerivedUUID
	   // container: tbox TerminologyBox
	   // contained: boxAxioms TerminologyBoxAxiom
	   val terminologyNestingAxiom = TerminologyNestingAxiom( uuid, tbox, nestingContext, nestingTerminology )
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
	   uuid: resolver.api.taggedTypes.TimeScalarRestrictionUUID,
	   tbox: resolver.api.TerminologyBox,
	   restrictedRange: resolver.api.DataRange,
	   minExclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LiteralDateTime],
	   minInclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LiteralDateTime],
	   maxExclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LiteralDateTime],
	   maxInclusive: scala.Option[gov.nasa.jpl.imce.oml.tables.LiteralDateTime],
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName )
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
	   uuid: resolver.api.taggedTypes.UnreifiedRelationshipUUID,
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
	   name: gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName )
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
	   uuid: resolver.api.taggedTypes.UnreifiedRelationshipInstanceTupleUUID,
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
	 		  
	 // UnreifiedRelationshipInversePropertyPredicate
	 override def createUnreifiedRelationshipInversePropertyPredicate
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.UnreifiedRelationshipInversePropertyPredicateUUID,
	   unreifiedRelationship: resolver.api.UnreifiedRelationship,
	   bodySegment: resolver.api.RuleBodySegment )
	 : (resolver.api.Extent, resolver.api.UnreifiedRelationshipInversePropertyPredicate)
	 = {
	   // factoryMethodWithImplicitlyDerivedUUID
	   // container: bodySegment RuleBodySegment
	   // contained: predicate SegmentPredicate
	   val unreifiedRelationshipInversePropertyPredicate = UnreifiedRelationshipInversePropertyPredicate( uuid, unreifiedRelationship, bodySegment )
	   scala.Tuple2(
	   	extent.copy(
	   	  predicate = extent.withSegmentPredicate(bodySegment, unreifiedRelationshipInversePropertyPredicate),
	   	  ruleBodySegmentOfSegmentPredicate = extent.ruleBodySegmentOfSegmentPredicate + (unreifiedRelationshipInversePropertyPredicate -> bodySegment),
	   	  segmentPredicateByUUID = extent.segmentPredicateByUUID + (uuid -> unreifiedRelationshipInversePropertyPredicate)),
	   	unreifiedRelationshipInversePropertyPredicate)
	 }
	 		  
	 // UnreifiedRelationshipPropertyPredicate
	 override def createUnreifiedRelationshipPropertyPredicate
	 ( extent: resolver.api.Extent,
	   uuid: resolver.api.taggedTypes.UnreifiedRelationshipPropertyPredicateUUID,
	   unreifiedRelationship: resolver.api.UnreifiedRelationship,
	   bodySegment: resolver.api.RuleBodySegment )
	 : (resolver.api.Extent, resolver.api.UnreifiedRelationshipPropertyPredicate)
	 = {
	   // factoryMethodWithImplicitlyDerivedUUID
	   // container: bodySegment RuleBodySegment
	   // contained: predicate SegmentPredicate
	   val unreifiedRelationshipPropertyPredicate = UnreifiedRelationshipPropertyPredicate( uuid, unreifiedRelationship, bodySegment )
	   scala.Tuple2(
	   	extent.copy(
	   	  predicate = extent.withSegmentPredicate(bodySegment, unreifiedRelationshipPropertyPredicate),
	   	  ruleBodySegmentOfSegmentPredicate = extent.ruleBodySegmentOfSegmentPredicate + (unreifiedRelationshipPropertyPredicate -> bodySegment),
	   	  segmentPredicateByUUID = extent.segmentPredicateByUUID + (uuid -> unreifiedRelationshipPropertyPredicate)),
	   	unreifiedRelationshipPropertyPredicate)
	 }
	 		  
}
