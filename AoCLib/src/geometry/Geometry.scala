package geometry

import exts.iterators.*
import coord.Pos

object Geometry:
  /** Calculates the area of a simple polygon using the shoelace formula.
    *
    * @see
    *   [[https://en.wikipedia.org/wiki/Shoelace_formula]]
    */
  def polygonArea(poss: Seq[Pos]): Long =
    ((poss.last +: poss).iterator.zipWithTail
      .map(_ cross _)
      .sum / 2L).abs
