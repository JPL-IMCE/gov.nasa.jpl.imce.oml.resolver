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

import java.io.File
import java.lang.System

import gov.nasa.jpl.imce.oml.tables.OMLSpecificationTables

import scala.{Array, StringContext, Unit}
import scala.Predef.{String, refArrayOps}
import scala.util.{Failure, Success}

object Test1 {

  def main(args: Array[String]): Unit
  = {
    System.out.println(s"args: ${args.length}")
    args.foreach { arg =>
      System.out.println(s"Arg: $arg")
    }

    if (args.length != 2) {
      System.err.println(s"Usage: ${args(0)} <OML Specification Tables ZIP file>")
      System.exit(-1)
    }

    run(args(1))
  }

  def run(omfSchemaJsonZipFile: String): Unit = {

    val factory = impl.OMLResolvedFactoryImpl()

    val result =
      for {
        tables <- OMLSpecificationTables.loadOMLSpecificationTables(new File(omfSchemaJsonZipFile))
        _ = System.out.println(s"... loaded tables")
        resolver <- OMLTablesResolver.resolve(tables, factory)
        _ = System.out.println(s"... resolved tables")
      } yield {
        System.out.println(s"...done!")
        System.out.println(s"valid? ${resolver.queue.isEmpty}")
        System.out.println(s"context graph:\n${resolver.context.g}")
        ()
      }

    result match {
      case Failure(f) =>
        throw f
      case Success(_) =>
        ()
    }

  }
}
