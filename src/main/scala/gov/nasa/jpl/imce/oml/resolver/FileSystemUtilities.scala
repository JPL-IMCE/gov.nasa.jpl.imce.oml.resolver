package gov.nasa.jpl.imce.oml.resolver

import ammonite.ops.{ls,up,Path}

import scala.collection.immutable.Seq
import scala.Boolean

/**
  * File system utilities for OML.
  */
object FileSystemUtilities {

  /**
    * Predicate for an OML OASIS Catalog XML file
    */
  val omlCatalogFilter = (p: Path) => p.isFile && p.segments.last == "oml.catalog.xml"

  /**
    * Recursively finds OML files of a certain kind from a given path
    *
    * @param p The path to explore recursively.
    * @param kindFilter A Path predicate for the kind of OML file to include in the result
    * @return A sequence of Path files satisfying the kindFilter found by recursively exploring `p`
    *         if it is a directory or `p`'s parent folder if it is a file named 'oml.catalog.xml'.
    *         In other cases, the result sequence is empty.
    */
  def lsRecOML
  (p: Path, kindFilter: Path => Boolean)
  : Seq[Path]
  = {
    def lsRec(dir: Path): Seq[Path] =
      ls
        .rec((p: Path) => p.isFile && !kindFilter(p))(dir)
        .filter(kindFilter).to[Seq]

    if (omlCatalogFilter(p))
      lsRec(p / up)
    else if (p.isDir)
      lsRec(p)
    else
      Seq.empty[Path]
  }

  /**
    * Predicate for an OML concrete syntax representation file in 4th normal database tabular form (*.omlzip)
    */
  def omlJsonZipFilePredicate(p: Path): Boolean = p.isFile && p.segments.last.endsWith(".omlzip")

  /**
    * Find *.omlzip files recursively from a directory path or the parent directory of an `oml.catalog.xml` file
    * @param p The directory or `oml.catalog.xml` file
    * @return The set of `*.omlzip` files found
    */
  def lsRecOMLJsonZipFiles
  (p: Path)
  : Seq[Path]
  = lsRecOML(p, kindFilter = omlJsonZipFilePredicate)

  /**
    * Predicate for an OML concrete syntax representation file in OML's Xtext DSL (*.oml)
    */
  def omlTextFilePredicate(p: Path): Boolean = p.isFile && p.segments.last.endsWith(".oml")

  /**
    * Find *.oml files recursively from a directory path or the parent directory of an `oml.catalog.xml` file
    * @param p The directory or `oml.catalog.xml` file
    * @return The set of `*.oml` files found
    */
  def lsRecOMLTextFiles
  (p: Path)
  : Seq[Path]
  = lsRecOML(p, kindFilter = omlTextFilePredicate)

  /**
    * Predicate for an OML concrete syntax representation file in W3C OWL2-DL + SWRL (*.owl)
    */
  def omlOWLFilePredicate(p: Path): Boolean = p.isFile && p.segments.last.endsWith(".owl")

  /**
    * Find *.owl files recursively from a directory path or the parent directory of an `oml.catalog.xml` file
    * @param p The directory or `oml.catalog.xml` file
    * @return The set of `*.owl` files found
    */
  def lsRecOMLOwlFiles
  (p: Path)
  : Seq[Path]
  = lsRecOML(p, kindFilter = omlOWLFilePredicate)

}
