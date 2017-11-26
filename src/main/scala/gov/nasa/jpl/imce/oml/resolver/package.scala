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

import java.util.UUID

import gov.nasa.jpl.imce.oml.covariantTag.@@
import gov.nasa.jpl.imce.oml.resolver.impl.OMLResolvedFactoryImpl
import gov.nasa.jpl.imce.oml.uuid.JVMUUIDGenerator

import scala.util.Try
import scala.Predef.String

package object resolver {

  implicit def toUUIDString[Tag](uuid: UUID @@ Tag)
  : String @@ Tag
  = covariantTag[Tag][String](uuid.toString)

  def initializeResolver
  ()
  : Try[OMLTablesResolver]
  = {
    val omlUUIDg = JVMUUIDGenerator()
    val factory = OMLResolvedFactoryImpl(omlUUIDg)
    val init = OMLTablesResolver.initializeTablesResolver(factory)
    Try(init)
  }

  def resolveTables
  (r: Try[OMLTablesResolver], ts: tables.OMLSpecificationTables)
  : Try[OMLTablesResolver]
  = for {
    current <- r
    prev = current.copy(queue = ts)
    updated <- OMLTablesResolver.resolve(prev)
    next <- OMLResolutionError.checkResolution(updated)
  } yield next


}
