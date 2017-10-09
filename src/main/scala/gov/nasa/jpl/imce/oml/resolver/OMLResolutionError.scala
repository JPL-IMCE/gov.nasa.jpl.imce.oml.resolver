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

import scala.util.{Failure,Try}
import scala.Predef.String

case class OMLResolutionError(message: String)
extends java.lang.IllegalArgumentException(message)

object OMLResolutionError {

  def checkResolution(resolver: OMLTablesResolver)
  : Try[OMLTablesResolver]
  = if (resolver.queue.isEmpty)
    Try(resolver)
  else {
    val message = "Incomplete resolution of OML Tables:\n" + resolver.queue.show
    Failure(OMLResolutionError(message))
  }
}