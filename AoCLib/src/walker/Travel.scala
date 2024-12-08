package walker

import coord.Pos
import grid.CharGrid

abstract class Travel(grid: CharGrid):
  def start: Walker
  def stopWalk(st: Walker): Boolean
  def isBlocked(st: Walker): Boolean
  def changeStateRule(st: Walker): Walker

  def traverse: (Set[Pos], Boolean) =
    import scala.collection.mutable
    val seen = mutable.Set.empty[Walker]
    var cur = start
    while !stopWalk(cur) && !seen.contains(cur) do
      seen.add(cur)
      cur = changeStateRule(cur)
    seen.map(_.p).toSet -> grid.contains(cur.p)
