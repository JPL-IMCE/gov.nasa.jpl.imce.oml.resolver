package gov.nasa.jpl.imce.oml.specification

import scala.collection.immutable.{SortedSet,TreeSet}

package object resolver {

  implicit def convertToAnnotations
  (apts: SortedSet[resolver.api.AnnotationPropertyTable])
  : SortedSet[resolver.api.Annotation]
  = apts
    .foldLeft[SortedSet[resolver.api.Annotation]](TreeSet.empty[resolver.api.Annotation]) { case (as1, apt) =>
      apt.value.foldLeft[SortedSet[resolver.api.Annotation]](as1) { case (as2, ae) =>
          as2 + impl.Annotation(terminology = ae.terminology, subject=ae.subject, property=apt.key, value=ae.value)
      }
  }
}
