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
package gov.nasa.jpl.imce.oml

import scala.collection.immutable.{SortedSet,TreeSet}

package object resolver {

  implicit def convertToAnnotations
  (apts: SortedSet[resolver.api.AnnotationPropertyTable])
  (implicit ex: resolver.api.Extent)
  : SortedSet[resolver.api.Annotation]
  = apts
    .foldLeft[SortedSet[resolver.api.Annotation]](TreeSet.empty[resolver.api.Annotation]) { case (as1, apt) =>
      apt.value.foldLeft[SortedSet[resolver.api.Annotation]](as1) { case (as2, ae) =>
        as2 + impl.Annotation(module = ae.module.uuid(ex), subject = ae.subject, property = apt.key, value = ae.value)
      }
  }

  def groupAnnotationsByProperty
  (as: SortedSet[resolver.api.Annotation])
  (implicit ex: resolver.api.Extent)
  : SortedSet[resolver.api.AnnotationPropertyTable]
  = as
    .groupBy(_.property)
    .foldLeft[SortedSet[resolver.api.AnnotationPropertyTable]](TreeSet.empty[resolver.api.AnnotationPropertyTable]) {
    case (acc, (ap, aes)) =>
      acc +
        resolver.impl.AnnotationPropertyTable(
          ap,
          aes
            .foldLeft[SortedSet[resolver.api.AnnotationEntry]](TreeSet.empty[resolver.api.AnnotationEntry]) {
            case (asi, a) =>
              ex.lookupModule(a.module).fold(asi) { am =>
                asi + resolver.impl.AnnotationEntry(am, a.subject, a.value)
              }
          })
  }
}
