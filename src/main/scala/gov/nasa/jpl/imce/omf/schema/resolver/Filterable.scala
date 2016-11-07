package gov.nasa.jpl.imce.omf.schema.resolver

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.Iterable
import scala.{AnyVal,Option,None,Nothing,PartialFunction,Some}

/**
  * This is inspired from QVT's select operator and 'select[<type>]' filtering syntax.
  */
object Filterable {

  class FilterableOption[U](@scala.transient val o: Option[U]) extends AnyVal {

    def selectByKindOf[V](pf: PartialFunction[U,V])
    : Option[V]
    = o.flatMap { u => if (pf.isDefinedAt(u)) Some(pf(u)) else None }

  }

  implicit def filterable[U](o: Option[U])
  : FilterableOption[U]
  = new FilterableOption(o)

  class FilterableIterable[U, F[X] <: Iterable[X]](@scala.transient val f: F[U]) extends AnyVal {

    def selectByKindOf[V]
    (pf: PartialFunction[U,V])
    (implicit cbf: CanBuildFrom[Nothing, V, F[V]])
    : F[V]
    = {
      val b = cbf.apply()
      f.foreach { u =>
        if (pf.isDefinedAt(u))
          b += pf(u)
      }
      b.result()
    }

  }

  implicit def filterable[U, F[U] <: Iterable[U]](f: F[U])
  : FilterableIterable[U, F]
  = new FilterableIterable[U, F](f)

}
