package gov.nasa.jpl.imce.omf.schema.resolver.impl

import scalax.collection.GraphEdge.{DiEdge, EdgeCopy, ExtendedKey, NodeProduct}
import scalax.collection.GraphPredef.OuterEdge
import scala.collection.immutable.Seq
import scala.{Product,StringContext}

case class TerminologyEdge[N]
(override val nodes: Product, tAxiom: TerminologyAxiom)
  extends DiEdge[N](nodes)
    with ExtendedKey[N]
    with EdgeCopy[TerminologyEdge]
    with OuterEdge[N, TerminologyEdge]
{
  def keyAttributes = Seq(tAxiom)
  override def copy[NN](newNodes: Product) = new TerminologyEdge[NN](newNodes, tAxiom)
  override protected def attributesToString = s" ${tAxiom}"
}

object TerminologyEdge {

  def apply
  (from: TerminologyBox, to: TerminologyBox, tAxiom:TerminologyAxiom)
  = new TerminologyEdge[TerminologyBox](NodeProduct(from, to), tAxiom)

}