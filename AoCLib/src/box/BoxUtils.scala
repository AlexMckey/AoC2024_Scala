package box

import coord.{Coord, given}
import scala.math.Ordering.Implicits.infixOrderingOps

trait Boxs[A : Coord : Ordering, B <: Boxs[A, B]](val dl: A, val ur: A):
  require(dl <= ur)
  def build: BoxFactory[A, B]

  def contains(pos: A)(using V: Coord[A])(using Ordering[V.Item]): Boolean =
    pos.in(dl, ur)

  def contains(that: Boxs[A, B])(using V: Coord[A])(using Ordering[V.Item]): Boolean =
    contains(that.dl) && contains(that.ur)

  def union(that: Boxs[A, B]): Boxs[A, B] =
    val unionMin = dl min that.dl
    val unionMax = ur max that.ur
    build(unionMin, unionMax)

  def intersect(that: Boxs[A, B]): Option[Boxs[A, B]] =
    val intersectMin = dl max that.dl
    val intersectMax = ur min that.ur
    if (intersectMin <= intersectMax)
      Some(build(intersectMin, intersectMax))
    else
      None

  def iterator: Iterator[A]

  def size[C : Numeric]: C

trait BoxFactory[A : Coord : Ordering, B <: Boxs[A, B]]:
  def apply(dl: A, ur: A): B

  //def apply(pos: A): B = apply(pos, pos)

  def bounding(poss: Iterable[A])(using V: Coord[A])(using N: Ordering[V.Item]): B =
    apply.tupled(Coord.boundingBox(poss))

