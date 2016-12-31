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

package gov.nasa.jpl.imce.omf.schema.resolver.impl

import gov.nasa.jpl.imce.omf.schema._

import scalax.collection.GraphEdge.{DiEdge, EdgeCopy, ExtendedKey, NodeProduct}
import scalax.collection.GraphPredef.OuterEdge
import scala.collection.immutable.Seq
import scala.{Product,StringContext,Tuple2}

case class TerminologyEdge[N]
(override val nodes: Product, tAxiom: resolver.api.TerminologyAxiom)
  extends DiEdge[N](nodes)
    with ExtendedKey[N]
    with EdgeCopy[TerminologyEdge]
    with OuterEdge[N, TerminologyEdge]
{
  def keyAttributes = Seq(tAxiom)
  override def copy[NN](newNodes: Product) = new TerminologyEdge[NN](newNodes, tAxiom)
  override protected def attributesToString = s" ${tAxiom}"

}

object TerminologyEdge {

  def apply
  (from: resolver.api.TerminologyBox,
   to: resolver.api.TerminologyBox,
   tAxiom:resolver.api.TerminologyAxiom)
  = new TerminologyEdge[TerminologyBox](NodeProduct(from, to), tAxiom)

  def replaceSource
  (e: TerminologyEdge[resolver.api.TerminologyBox],
   thatSource: resolver.api.TerminologyBox)
  : TerminologyEdge[resolver.api.TerminologyBox]
  = new TerminologyEdge[resolver.api.TerminologyBox](
    Tuple2(thatSource, e.target),
    replaceAxiomSource(e.tAxiom, thatSource))

  def replaceTarget
  (e: TerminologyEdge[resolver.api.TerminologyBox],
   thatTarget: resolver.api.TerminologyBox)
  : TerminologyEdge[resolver.api.TerminologyBox]
  = new TerminologyEdge[resolver.api.TerminologyBox](
    Tuple2(e.source, thatTarget),
    replaceAxiomTarget(e.tAxiom, thatTarget))

  def replaceAxiomSource
  (tAxiom: resolver.api.TerminologyAxiom,
   thatSource: resolver.api.TerminologyBox)
  : resolver.api.TerminologyAxiom
  = tAxiom match {
    case tx: TerminologyExtensionAxiom =>
      tx.copy(extendingTerminology = thatSource)
    case tx: ConceptDesignationTerminologyAxiom =>
      thatSource match {
        case thatGraph: resolver.api.TerminologyGraph =>
          tx.copy(designationTerminologyGraph = thatGraph)
        case _ =>
          throw new java.lang.IllegalArgumentException(
            "replaceAxiomSource for a ConceptualDesignationTerminologyAxiom must be a TerminologyGraph!")
      }
    case tx: TerminologyNestingAxiom =>
      thatSource match {
        case thatGraph: resolver.api.TerminologyGraph =>
          tx.copy(nestedTerminology = thatGraph)
        case _ =>
          throw new java.lang.IllegalArgumentException(
            "replaceAxiomSource for a TerminologyNestingAxiom must be a TerminologyGraph!")
      }
    case tx: BundledTerminologyAxiom =>
      thatSource match {
        case thatBundle: resolver.api.Bundle =>
          tx.copy(terminologyBundle = thatBundle)
        case _ =>
          throw new java.lang.IllegalArgumentException(
            "replaceAxiomSource for a BundledTerminologyAxiom must be a TerminologyGraph!")
      }
  }

  def replaceAxiomTarget
  (tAxiom: resolver.api.TerminologyAxiom,
   thatTarget: resolver.api.TerminologyBox)
  : resolver.api.TerminologyAxiom
  = tAxiom match {
    case tx: TerminologyExtensionAxiom =>
      tx.copy(extendedTerminology = thatTarget)
    case tx: ConceptDesignationTerminologyAxiom =>
      tx.copy(designatedTerminology = thatTarget)
    case tx: TerminologyNestingAxiom =>
      tx.copy(nestingTerminology = thatTarget)
    case tx: BundledTerminologyAxiom =>
      tx.copy(bundledTerminology = thatTarget)
  }

}