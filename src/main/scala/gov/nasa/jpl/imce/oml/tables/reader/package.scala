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

package gov.nasa.jpl.imce.oml.tables

import ammonite.ops.Path
import org.apache.commons.compress.archivers.zip.ZipFile

import scala.collection.immutable.Seq
import scala.collection.JavaConversions._

package object reader {

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
    *
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
