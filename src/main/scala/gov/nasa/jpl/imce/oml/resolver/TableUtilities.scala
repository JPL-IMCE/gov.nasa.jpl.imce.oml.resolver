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

import gov.nasa.jpl.imce.oml.tables.{taggedTypes,OMLSpecificationTables}
import scala.collection.immutable.{Map, Seq}
import scala.collection.JavaConversions._
import scala.Predef.ArrowAssoc

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
