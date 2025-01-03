package intervals

import scala.math.Numeric.Implicits.given
import scala.math.Ordering.Implicits.given
import scala.annotation.tailrec

class Intervals[A] private (private[intervals] val pairs: List[(A, A)])(using n: Numeric[A]):
  def |(other: Intervals[A]): Intervals[A] =
    @tailrec
    def merge(unmerged: List[(A, A)], merged: List[(A, A)]): List[(A, A)] = unmerged match
      case Nil => merged.reverse
      case head :: Nil => (head :: merged).reverse
      case left :: right :: tail =>
        if left._2 >= right._1 - n.one then
          val newEnd = if left._2 >= right._2 then left._2 else right._2
          merge((left._1, newEnd) :: tail, merged)
        else
          merge(right :: tail, left :: merged)

    new Intervals(merge((pairs ++ other.pairs).sorted, List.empty))

  def -(other: Intervals[A]): Intervals[A] =
    @tailrec
    def subtract(previous: List[(A, A)], remove: List[(A, A)], result: List[(A, A)]): List[(A, A)] = (previous, remove) match
      // Ran out of segments
      case (Nil, _) =>
        result.reverse

      // Ran out of removals
      case (pair :: tail, Nil) =>
        subtract(tail, Nil, pair :: result)

      // completely erase segment
      case ((lStart, lEnd) :: lTail, (rStart, rEnd) :: rTail) if rStart <= lStart && rEnd >= lEnd =>
        subtract(lTail, remove, result)

      // erase the left part of the segment
      case ((lStart, lEnd) :: lTail, (rStart, rEnd) :: rTail) if rStart <= lStart && rEnd >= lStart =>
        subtract((rEnd + n.one, lEnd) :: lTail, rTail, result)

      // erase the right part of the segment
      case ((lStart, lEnd) :: lTail, (rStart, rEnd) :: rTail) if rStart >= lStart && rEnd >= lStart =>
        subtract(lTail, remove, (lStart, rStart - n.one) :: result)

      // Last 3 cases have not been tested in a puzzle
      // erase the middle part of the segment
      case ((lStart, lEnd) :: lTail, (rStart, rEnd) :: rTail) if rStart >= lStart && rEnd <= lEnd =>
        subtract((rEnd + n.one, lEnd) :: lTail, rTail, (lStart, rStart - n.one) :: result)

      // doesn't overlap and remove is lower than segment
      case ((lStart, lEnd) :: lTail, (rStart, rEnd) :: rTail) if rEnd < lStart =>
        subtract(previous, rTail, result)

      // doesn't overlap and segment is lower than remove
      case ((lStart, lEnd) :: lTail, _) =>
        subtract(lTail, remove, (lStart, lEnd) :: result)

    new Intervals(subtract(pairs, other.pairs, List.empty))

  def elements(using Integral[A]): List[A] =
    pairs.flatMap((start, end) => List.range(start, end + n.one))

  def contains(element: A): Boolean =
    pairs.exists((start, end) => element >= start && element <= end)

  def size: A = pairs.map((start, end) => end - start + n.one).sum

  override def toString: String =
    pairs.map((start, end) => s"[$start .. $end]").mkString(", ")

  def flatMap[B : Numeric](f: (A, A) => Intervals[B]): Intervals[B] =
    if isEmpty then
      Intervals.empty[B]
    else
      pairs.map(f.tupled).reduceLeft(_ | _)

  def isEmpty: Boolean =
    pairs.isEmpty

  def min: A =
    pairs.head._1

  def max: A =
    pairs.last._2

  def trimBelow(a: A): Intervals[A] =
    if !isEmpty && min < a then
      this - Intervals(min, a - n.one)
    else
      this

  def trimAbove(a: A): Intervals[A] =
    if !isEmpty && max > a then
      this - Intervals(a + n.one, max)
    else
      this

  def splitAt(a: A): (Intervals[A], Intervals[A]) =
    trimAbove(a - n.one) -> trimBelow(a)

object Intervals:
  def apply[A : Numeric](start: A, end: A): Intervals[A] =
    new Intervals(List(start -> end))

  def apply[A : Numeric](pair: (A, A)): Intervals[A] =
    new Intervals(List(pair))

  def empty[A : Numeric]: Intervals[A] =
    new Intervals(List.empty)

extension [A : Numeric] (number: A)
  def +-(other: A): Intervals[A] =
    Intervals(number - other, number + other)
