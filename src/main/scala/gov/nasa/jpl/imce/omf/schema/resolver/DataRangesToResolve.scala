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

package gov.nasa.jpl.imce.omf.schema.resolver

import java.util.UUID
import gov.nasa.jpl.imce.omf._

import scala.{Boolean,Tuple2,Tuple3}
import scala.collection.immutable.{Map,Seq,Set}
import scala.util.{Failure,Success,Try}
import scala.Predef.ArrowAssoc

/**
  * Each field is a map from: Graph UUID to a map of (restricted range UUID, dataRange)
  * @param binaryScalarRestrictions
  * @param iriScalarRestrictions
  * @param numericScalarRestrictions
  * @param plainLiteralScalarRestrictions
  * @param scalarOneOfRestrictions
  * @param stringScalarRestrictions
  * @param timeScalarRestrictions
  */
case class DataRangesToResolve
( binaryScalarRestrictions: Map[UUID, Seq[(UUID, schema.tables.BinaryScalarRestriction)]],
  iriScalarRestrictions: Map[UUID, Seq[(UUID, schema.tables.IRIScalarRestriction)]],
  numericScalarRestrictions: Map[UUID, Seq[(UUID, schema.tables.NumericScalarRestriction)]],
  plainLiteralScalarRestrictions: Map[UUID, Seq[(UUID, schema.tables.PlainLiteralScalarRestriction)]],
  scalarOneOfRestrictions: Map[UUID, Seq[(UUID, schema.tables.ScalarOneOfRestriction)]],
  stringScalarRestrictions: Map[UUID, Seq[(UUID, schema.tables.StringScalarRestriction)]],
  timeScalarRestrictions: Map[UUID, Seq[(UUID, schema.tables.TimeScalarRestriction)]] )

object DataRangesToResolve {

  val empty = DataRangesToResolve(
    binaryScalarRestrictions = Map.empty,
    iriScalarRestrictions = Map.empty,
    numericScalarRestrictions = Map.empty,
    plainLiteralScalarRestrictions = Map.empty,
    scalarOneOfRestrictions = Map.empty,
    stringScalarRestrictions = Map.empty,
    timeScalarRestrictions = Map.empty)

  final def resolve(resolver: OMFSchemaResolver, queue: DataRangesToResolve)
  : Try[(OMFSchemaResolver, DataRangesToResolve)]
  = {

    val r0 = resolver
    val q0 = DataRangesToResolve.empty
    val f0 = false

    val resolved = for {
      step1 <- queue.binaryScalarRestrictions
        .foldLeft[Try[(OMFSchemaResolver, DataRangesToResolve, Boolean)]](Success(Tuple3(r0, q0, f0))) {
        case (Success(Tuple3(ri, qi, fi)), (guuid, drs)) =>
          val gi = ri.context.g
          val tbox = ri.context.nodes(guuid)
          val accessible = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.dataranges).toMap
          val (available, remaining) = drs.partition { case (uuid, dr) => accessible.contains(uuid) }
          val si = available
            .map { case (ruuid, dr) =>
              impl.BinaryScalarRestriction(
                UUID.fromString(dr.uuid),
                dr.name,
                dr.iri,
                dr.length,
                dr.maxLength,
                dr.minLength,
                accessible(ruuid))
            }
            .to[Set]
          impl.TerminologyContext
            .replaceNode(gi, tbox, tbox.withBoxStatements(si))
            .map { gj =>
              Tuple3(
                ri.copy(context = impl.TerminologyContext(gj)),
                qi.copy(binaryScalarRestrictions = qi.binaryScalarRestrictions + (guuid -> remaining)),
                fi || si.nonEmpty)
            }
        case (Failure(t), _) =>
          Failure(t)
      }

      step2 <- queue.iriScalarRestrictions
        .foldLeft[Try[(OMFSchemaResolver, DataRangesToResolve, Boolean)]](Success(step1)) {
        case (Success(Tuple3(ri, qi, fi)), (guuid, drs)) =>
          val gi = ri.context.g
          val tbox = ri.context.nodes(guuid)
          val accessible = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.dataranges).toMap
          val (available, remaining) = drs.partition { case (uuid, dr) => accessible.contains(uuid) }
          val si = available
            .map { case (ruuid, dr) =>
              impl.IRIScalarRestriction(
                UUID.fromString(dr.uuid),
                dr.name,
                dr.iri,
                dr.length,
                dr.maxLength,
                dr.minLength,
                dr.pattern,
                accessible(ruuid))
            }
            .to[Set]
          impl.TerminologyContext
            .replaceNode(gi, tbox, tbox.withBoxStatements(si))
            .map { gj =>
              Tuple3(
                ri.copy(context = impl.TerminologyContext(gj)),
                qi.copy(iriScalarRestrictions = qi.iriScalarRestrictions + (guuid -> remaining)),
                fi || si.nonEmpty)
            }
        case (Failure(t), _) =>
          Failure(t)
      }

      step3 <- queue.numericScalarRestrictions
        .foldLeft[Try[(OMFSchemaResolver, DataRangesToResolve, Boolean)]](Success(step2)) {
        case (Success(Tuple3(ri, qi, fi)), (guuid, drs)) =>
          val gi = ri.context.g
          val tbox = ri.context.nodes(guuid)
          val accessible = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.dataranges).toMap
          val (available, remaining) = drs.partition { case (uuid, dr) => accessible.contains(uuid) }
          val si = available
            .map { case (ruuid, dr) =>
              impl.NumericScalarRestriction(
                UUID.fromString(dr.uuid),
                dr.name,
                dr.iri,
                dr.maxExclusive,
                dr.maxInclusive,
                dr.minExclusive,
                dr.minInclusive,
                accessible(ruuid))
            }
            .to[Set]
          impl.TerminologyContext
            .replaceNode(gi, tbox, tbox.withBoxStatements(si))
            .map { gj =>
              Tuple3(
                ri.copy(context = impl.TerminologyContext(gj)),
                qi.copy(numericScalarRestrictions = qi.numericScalarRestrictions + (guuid -> remaining)),
                fi || si.nonEmpty)
            }
        case (Failure(t), _) =>
          Failure(t)
      }

      step4 <- queue.plainLiteralScalarRestrictions
        .foldLeft[Try[(OMFSchemaResolver, DataRangesToResolve, Boolean)]](Success(step3)) {
        case (Success(Tuple3(ri, qi, fi)), (guuid, drs)) =>
          val gi = ri.context.g
          val tbox = ri.context.nodes(guuid)
          val accessible = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.dataranges).toMap
          val (available, remaining) = drs.partition { case (uuid, dr) => accessible.contains(uuid) }
          val si = available
            .map { case (ruuid, dr) =>
              impl.PlainLiteralScalarRestriction(
                UUID.fromString(dr.uuid),
                dr.name,
                dr.iri,
                dr.language,
                dr.length,
                dr.maxLength,
                dr.minLength,
                dr.pattern,
                accessible(ruuid))
            }
            .to[Set]
          impl.TerminologyContext
            .replaceNode(gi, tbox, tbox.withBoxStatements(si))
            .map { gj =>
              Tuple3(
                ri.copy(context = impl.TerminologyContext(gj)),
                qi.copy(plainLiteralScalarRestrictions = qi.plainLiteralScalarRestrictions + (guuid -> remaining)),
                fi || si.nonEmpty)
            }
        case (Failure(t), _) =>
          Failure(t)
      }

      step5 <- queue.scalarOneOfRestrictions
        .foldLeft[Try[(OMFSchemaResolver, DataRangesToResolve, Boolean)]](Success(step4)) {
        case (Success(Tuple3(ri, qi, fi)), (guuid, drs)) =>
          val gi = ri.context.g
          val tbox = ri.context.nodes(guuid)
          val accessible = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.dataranges).toMap
          val (available, remaining) = drs.partition { case (uuid, dr) => accessible.contains(uuid) }
          val si = available
            .map { case (ruuid, dr) =>
              impl.ScalarOneOfRestriction(
                UUID.fromString(dr.uuid),
                dr.name,
                dr.iri,
                accessible(ruuid))
            }
            .to[Set]
          impl.TerminologyContext
            .replaceNode(gi, tbox, tbox.withBoxStatements(si))
            .map { gj =>
              Tuple3(
                ri.copy(context = impl.TerminologyContext(gj)),
                qi.copy(scalarOneOfRestrictions = qi.scalarOneOfRestrictions + (guuid -> remaining)),
                fi || si.nonEmpty)
            }
        case (Failure(t), _) =>
          Failure(t)
      }

      step6 <- queue.stringScalarRestrictions
        .foldLeft[Try[(OMFSchemaResolver, DataRangesToResolve, Boolean)]](Success(step5)) {
        case (Success(Tuple3(ri, qi, fi)), (guuid, drs)) =>
          val gi = ri.context.g
          val tbox = ri.context.nodes(guuid)
          val accessible = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.dataranges).toMap
          val (available, remaining) = drs.partition { case (uuid, dr) => accessible.contains(uuid) }
          val si = available
            .map { case (ruuid, dr) =>
              impl.StringScalarRestriction(
                UUID.fromString(dr.uuid),
                dr.name,
                dr.iri,
                dr.length,
                dr.maxLength,
                dr.minLength,
                dr.pattern,
                accessible(ruuid))
            }
            .to[Set]
          impl.TerminologyContext
            .replaceNode(gi, tbox, tbox.withBoxStatements(si))
            .map { gj =>
              Tuple3(
                ri.copy(context = impl.TerminologyContext(gj)),
                qi.copy(stringScalarRestrictions = qi.stringScalarRestrictions + (guuid -> remaining)),
                fi || si.nonEmpty)
            }
        case (Failure(t), _) =>
          Failure(t)
      }

      step7 <- queue.timeScalarRestrictions
        .foldLeft[Try[(OMFSchemaResolver, DataRangesToResolve, Boolean)]](Success(step6)) {
        case (Success(Tuple3(ri, qi, fi)), (guuid, drs)) =>
          val gi = ri.context.g
          val tbox = ri.context.nodes(guuid)
          val accessible = gi.outerNodeTraverser(gi.get(tbox)).flatMap(_.dataranges).toMap
          val (available, remaining) = drs.partition { case (uuid, dr) => accessible.contains(uuid) }
          val si = available
            .map { case (ruuid, dr) =>
              impl.TimeScalarRestriction(
                UUID.fromString(dr.uuid),
                dr.name,
                dr.iri,
                dr.maxExclusive,
                dr.maxInclusive,
                dr.minExclusive,
                dr.minInclusive,
                accessible(ruuid))
            }
            .to[Set]
          impl.TerminologyContext
            .replaceNode(gi, tbox, tbox.withBoxStatements(si))
            .map { gj =>
              Tuple3(
                ri.copy(context = impl.TerminologyContext(gj)),
                qi.copy(timeScalarRestrictions = qi.timeScalarRestrictions + (guuid -> remaining)),
                fi || si.nonEmpty)
            }
        case (Failure(t), _) =>
          Failure(t)
      }
    } yield step7

    resolved match {
      case Success(Tuple3(r7, q7, f7)) =>
        if (f7)
          resolve(r7, q7)
        else
          Success(Tuple2(r7, q7))
      case Failure(t) =>
        Failure(t)
    }
  }
}