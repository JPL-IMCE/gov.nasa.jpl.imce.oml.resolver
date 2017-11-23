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

package gov.nasa.jpl.imce.oml.resolver.impl

import gov.nasa.jpl.imce.oml._

trait Module
extends resolver.api.Module
  with Resource
{
  override val uuid: resolver.api.taggedTypes.ModuleUUID
  override val iri: gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI

  override def iri
  ()(implicit extent: resolver.api.Extent)
	  : scala.Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI]
	  = {
	    scala.Some(iri)
	  }

  def nsPrefix
  ()
	  : gov.nasa.jpl.imce.oml.tables.taggedTypes.NamespacePrefix
	  = {
	    gov.nasa.jpl.imce.oml.tables.taggedTypes.namespacePrefix(iri.substring(1+iri.lastIndexOf('/')))
	  }

  def name
  ()
	  : gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName
	  = {
	    gov.nasa.jpl.imce.oml.tables.taggedTypes.localName(nsPrefix)
	  }

  def abbrevIRI
  ()(implicit extent: resolver.api.Extent)
	  : scala.Option[scala.Predef.String]
	  = {
	    scala.Some(nsPrefix)
	  }

  def moduleContext
  ()(implicit extent: resolver.api.Extent)
	  : scala.Option[resolver.api.Module]
	  = {
	    scala.Some(this)
	  }

  override def canEqual(that: scala.Any): scala.Boolean = that match {
	  case _: Module => true
 	  case _ => false
  }

}
