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

case class BundledTerminologyAxiom private[impl] 
(
 override val uuid: java.util.UUID,
 override val bundledTerminology: resolver.api.TerminologyBox
)
extends resolver.api.BundledTerminologyAxiom
  with TerminologyBundleAxiom
{
		
  /*
   * The bundle is the source
   */
  override def source
  ()(implicit extent: resolver.api.Extent)
  : scala.Option[resolver.api.TerminologyBox]
  = {
    extent.bundleOfTerminologyBundleAxiom.get(this)
  }
  
  /*
   * The bundledTerminology is the target
   */
  override def target
  ()(implicit extent: resolver.api.Extent)
  : resolver.api.TerminologyBox
  = {
    bundledTerminology
  }
  



  override def canEqual(that: scala.Any): scala.Boolean = that match {
  	case _: BundledTerminologyAxiom => true
  	case _ => false
  }

  override val hashCode
  : scala.Int
  = (uuid, bundledTerminology).##

  override def equals(other: scala.Any): scala.Boolean = other match {
	  case that: BundledTerminologyAxiom =>
	    (that canEqual this) &&
	    (this.uuid == that.uuid) &&
	    (this.bundledTerminology == that.bundledTerminology)

	  case _ =>
	    false
  }
}
