package test

import java.io.File
import java.lang.System

import gov.nasa.jpl.imce.omf.schema.resolver.OMFSchemaResolver
import gov.nasa.jpl.imce.omf.schema.tables.OMFSchemaTables

import scala.Predef.refArrayOps
import scala.util.{Failure, Success}
import scala.{Array,StringContext,Unit}
import scala.Predef.String

object Test1 {

  def main(args: Array[String]): Unit
  = {
    System.out.println(s"args: ${args.length}")
    args.foreach { arg =>
      System.out.println(s"Arg: $arg")
    }

    if (args.length != 2) {
      System.err.println(s"Usage: ${args(0)} <OMF Schema table ZIP file>")
      System.exit(-1)
    }

    run(args(1))
  }

  def run(omfSchemaJsonZipFile: String): Unit = {

    val result =
      for {
        tables <- OMFSchemaTables.loadOMFSchemaTables(new File(omfSchemaJsonZipFile))
        _ = System.out.println(s"... loaded tables")
        resolver <- OMFSchemaResolver.resolve(tables)
        _ = System.out.println(s"... resolved tables")
      } yield {
        System.out.println(s"...done!")
        System.out.println(s"valid? ${resolver.invalid.isEmpty}")
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