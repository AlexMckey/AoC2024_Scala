package coord

import coord.{Coord, Pos, given}

trait Neighbor[A]:
  def neighbors(p: Pos, a: A): Iterator[Pos]

given nearNeighbors[A]: Neighbor[A] with
  override def neighbors(p: Pos, a: A): Iterator[Pos] = p.nearAll

given axisNeighbors[A]: Neighbor[A] with
  override def neighbors(p: Pos, a: A): Iterator[Pos] = p.nearAxis