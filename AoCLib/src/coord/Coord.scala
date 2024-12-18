package coord

import math.gcd

trait Coord[C]:
  type Item

  def build(xs: Seq[Item]): C

  val size: Int

  def axes: Range = 0 until size

  def zip(a: C, b: C)(f: (Item, Item) => Item): C =
    build(for (a, b) <- a.axis zip b.axis yield f(a, b))

  extension (a: C)
    def axis: Seq[Item]
    def at(i: Int): Item = a.axis(i)
    def show: String = a.axis.mkString("(", "; ", ")")
    def map(f: Item => Item): C = build(a.axis map f)
    def mapWithIndex(f: (Item, Int) => Item): C = build(a.axis.zipWithIndex map f.tupled)
    def update(i: Int, item: Item): C = mapAt(i, _ => item)
    def mapAt(i: Int, f: Item => Item): C =
      build(axis.zipWithIndex.map {
        case (c, `i`) => f(c)
        case (c,   _) => c
      })

  extension (a: C)(using N: Numeric[Item])
    def +(b: C): C = zip(a, b)(N.plus)
    def *(b: C): C = zip(a, b)(N.times)
    def *(f: Item)(using DummyImplicit): C = a.map(N.times(_, f))
    def unary_- : C = a.map(N.negate)
    def -(b: C): C = a + -b
    def norm: Item = a.axis.map(N.abs).sum
    infix def manhattan(b: C): Item = zip(a,b)(N.minus).norm
    def nearAll: Iterator[C] = all.filter(_ != a)
    def all: Iterator[C] = Coord.cubeAt(a)(using this)
    def nearAxis: Iterator[C] =
      for
        d <- Iterator(N.one, N.negate(N.one))
        i <- axes
      yield a.mapAt(i, N.plus(_, d))
    def multiple: Item = a.axis.product

  extension (a: C)(using N: Integral[Item])
    def /(f: Item): C = a.map(N.quot(_, f))
    def %(f: Item): C = a.map(N.rem(_, f))
    def |%|(b: C): C = zip(a, b)(N.rem)
    def unite: C = a / a.axis.filter(_ != N.zero).map(N.abs).min

  extension (a: C)(using O: Ordering[Item])
    def in(cube: (C, C)): Boolean =
      import O._
      (cube._1.axis zip a.axis zip cube._2.axis).forall {
        case ((l, m), r) => l <= m && m <= r
      }

object Coord:
  def apply[C](using C: Coord[C]): C.type = C

  def zero[C](using V: Coord[C])(using N: Numeric[V.Item]): C =
    V.build(Iterator.continually(N.zero).take(V.size).toSeq)

  def one[C](using V: Coord[C])(using N: Numeric[V.Item]): C =
    V.build(Iterator.continually(N.one).take(V.size).toSeq)

  def of[C](s: String)(using V: Coord[C])(using N: Numeric[V.Item]): C =
    V.build(s.split(",\\s*").map(_.toInt).toSeq.map(N.fromInt))

  def cubeAt[C](center: C)(using V: Coord[C])(using N: Numeric[V.Item]): Iterator[C] =
    val values = Seq(N.one, N.zero, N.negate(N.one))
    center
      .axis
      .foldLeft(Iterator(List.empty[V.Item])) { (it, c) =>
        for
          tail <- it
          d <- values
        yield N.plus(d, c) :: tail
      }
      .map(_.reverse)
      .map(V.build)

  def boundingBox[C](xs: Iterable[C])(using V: Coord[C])(using N: Ordering[V.Item]): (C, C) =
    xs.foldLeft(xs.head, xs.head) {
      case ((min, max), x) => (
        V.zip(min, x)(N.min),
        V.zip(max, x)(N.max)
      )
    }

/** @returns minimal vector S that A + K*S = B */
def discreteStepTowards[C](a: C, b: C)(using V: Coord[C])(using N: Integral[V.Item]): C =
  import N.*
  val deltas = V.zip(b, a)(_ - _)
  if deltas.axis.forall(_ == N.zero) then deltas
  else
    val divisor = deltas.axis.map(abs).reduce(gcd)
    deltas.map(_ / divisor)

def discreteLine[C](a: C, b: C)(using V: Coord[C])(using N: Integral[V.Item]): Iterator[C] =
  val step = discreteStepTowards(a, b)
  Iterator.iterate(a)(_ + step).takeWhile(_ != b) ++ Iterator(b)

final case class Dot(i: Int) extends Coord[Int]:
  type Item = Int
  extension (v: Int) def axis: Seq[Int] = Seq(v)
  def build(xs: Seq[Item]): Int = xs.head
  val size: Int = 1

given vecOrdering[C](using V: Coord[C])(using O: Ordering[V.Item]): scala.math.Ordering[C] with
  //import math.Ordered.orderingToOrdered
  import scala.math.Ordering.Implicits.seqOrdering
  override def compare(x: C, y: C): Int =
    Ordering[Seq[V.Item]].compare(x.axis, y.axis)
//    x.axis.zip(y.axis)
//      .map(O.compare)
//      .collectFirst { case x if x != 0 => x}
//      .getOrElse(0)