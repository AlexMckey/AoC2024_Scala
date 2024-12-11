package coord

import coord.{Coord, Pos, given}

trait Neighbors[A]:
  def neighbors(p: Pos, a: A): Iterator[Pos]

given nearNeighbors[A]: Neighbors[A] with
  override def neighbors(p: Pos, a: A): Iterator[Pos] = p.nearAll

given axisNeighbors[A]: Neighbors[A] with
  override def neighbors(p: Pos, a: A): Iterator[Pos] = p.nearAxis