package coord

import coord.{Coord, Pos, given}
import exts.maps.A

trait Neighbors[A]:
  def neighbors(p: Pos, a: A): Iterator[Pos]
  
object NearNeighbors:
  given Neighbors[A] with
    override def neighbors(p: Pos, a: A): Iterator[Pos] = p.nearAll

object AxisNeighbors:
  given Neighbors[A] with
    override def neighbors(p: Pos, a: A): Iterator[Pos] = p.nearAxis