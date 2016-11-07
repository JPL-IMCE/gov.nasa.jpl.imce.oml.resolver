package gov.nasa.jpl.imce.omf.schema.resolver

import scala.collection.immutable.Iterable
import scala.Option

package object impl {

  import gov.nasa.jpl.imce.omf.schema.resolver.Filterable._

  implicit def filterable[U](o: Option[U])
  : FilterableOption[U]
  = new FilterableOption(o)

  implicit def filterable[U, F[U] <: Iterable[U]](f: F[U])
  : FilterableIterable[U, F]
  = new FilterableIterable[U, F](f)

}
