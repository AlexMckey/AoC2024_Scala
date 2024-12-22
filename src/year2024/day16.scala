package year2024.day16

import year2024.DayOf2024
import parse.{*, given}
import grid.{CharGrid, VectorGrid}
import coord.{Dir, Pos}
import graph.traverse.Dijkstra
import walker.Walker

case class Maze(input: String):
  private lazy val g: CharGrid = VectorGrid(input)
  private lazy val start: Walker = Walker(g.posOf('S').get, Dir.E)
  private lazy val endPos: Pos = g.posOf('E').get
  private lazy val (Some(goal, steps), path): (Option[(Walker, Int)], Map[Walker, Int]) =
    Dijkstra(start, _.p == endPos)(ns(_.step)) : @unchecked

  private def ns(go: Walker => Walker)(w: Walker): Set[(Walker, Int)] =
    val newW = go(w)
    Set(w.turnLeft -> 1000, w.turnRight -> 1000) ++
      (if g(newW.p) != '#' then Set(newW -> 1) else Set.empty)

  private def backPaths(w: Walker): Set[Pos] =
    if w == start
    then Set(w.p)
    else
      val dist = path(w)
      val ps =
        for
          (oldW, step) <- ns(_.prev)(w)
          oldDist <- path.get(oldW)
          if oldDist + step == dist
        yield backPaths(oldW) + w.p
      ps.foldLeft(Set.empty)(_ ++ _)

  def stepCnt: Int = steps

  def bestPathCnt: Int = backPaths(goal).size

given Read[Maze] = Maze(_)

object Day16 extends DayOf2024[Maze](16, "Reindeer Maze"):

  override def part1(m: Maze): Long =
    m.stepCnt

  override def part2(m: Maze): Long =
    m.bestPathCnt

//Day 16: Reindeer Maze
//  parse : 17.5ms
//  part 1: 254.ms -> 90440
//  part 2: 125.ms -> 479