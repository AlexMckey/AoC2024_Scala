package year2024.day20

import year2024.DayOf2024
import parse.{*, given}
import grid.{CharGrid, VectorGrid}
import coord.{pos, Pos}
import graph.traverse.BFS

case class RaceGrid(s: String):
  lazy val g: CharGrid = VectorGrid(s)
  lazy val start: Pos = g.posOf('S').get
  lazy val end: Pos = g.posOf('E').get
  def ns(p: Pos): Iterable[Pos] =
    p.nearAxis.filter(g.contains).filterNot(g(_) == '#').toSeq
  lazy val (Some(_), path) = BFS.search(start, end)(ns) : @unchecked
  def cheatSaves(cheat: Int => Boolean): Iterator[Int] =
    for
      (p1, d1) <- path.iterator
      (p2, d2) <- path.iterator
      d12 = p1.manhattan(p2)
      if cheat(d12)
    yield d2 - d1 - d12

given Read[RaceGrid] = RaceGrid(_)

object Day20 extends DayOf2024[RaceGrid](20, "Race Condition"):

  override def part1(i: RaceGrid): Long =
    i.cheatSaves(_ == 2).count(_ >= 100)

  override def part2(i: RaceGrid): Long =
    i.cheatSaves(_ <= 20).count(_ >= 100)

//Day 20: Race Condition
//  parse : 0.66ms
//  part 1: 13.7s -> 1321
//  part 2: 13.3s -> 971737
