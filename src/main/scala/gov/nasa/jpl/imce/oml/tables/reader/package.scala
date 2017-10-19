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
    * Read `*.oml.json.zip` files and aggregate the OML tables from each file.
    *
    * Note: The aggregation is done in parallel across all `*.oml.json.zip` files
    * and across all OML tables within each file.
    *
    * @param omlZips A set of `*.oml.json.zip` files to read
    * @return The aggregated OML tables read from all `*.oml.json.zip` files.
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
          // @TODO Investigate why this causes out of memory errors when running zeppeling in docker
          // .par
          .aggregate(prev)(
            seqop = OMLSpecificationTables.readZipArchive(zip),
            combop = OMLSpecificationTables.mergeTables)

      zip.close()

      next
    }

    omlZips
      // @TODO Investigate why this causes out of memory errors when running zeppeling in docker
      // .par
      .aggregate[OMLSpecificationTables](
      OMLSpecificationTables.createEmptyOMLSpecificationTables()
    )(seqop=readOMLZipFile, combop=OMLSpecificationTables.mergeTables)
  }

}
