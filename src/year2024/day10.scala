package year2024.day10

import year2024.DayOf2024
import parse.{*, given}
import coord.{Pos, given}
import grid.{CharGrid, MapGrid}

import scala.collection.mutable

case class TopographicMap(s: String):
  lazy val g: CharGrid = MapGrid(s)
  lazy val starts: Seq[Pos] = g.filter(_ == '0').allPos.toList
  private lazy val ends: Set[Pos] = g.filter(_ == '9').allPos

  private def neighbors(p: Pos): Iterator[Pos] =
    p.nearAxis.filter(g.contains).filter(n => g(n) - g(p) == 1)

  private def traverse[A](start: A, neighbors: A => IterableOnce[A]): Map[A, Int] =
    val visitedCnt: mutable.Map[A, Int] = mutable.Map.empty.withDefaultValue(0)
    val toVisit: mutable.Queue[A] = mutable.Queue.empty
    toVisit.enqueue(start)
    while toVisit.nonEmpty do
      val node = toVisit.dequeue()
      visitedCnt(node) += 1
      toVisit.appendAll(neighbors(node))
    end while
    visitedCnt.toMap
  end traverse

  lazy val trailheads: Seq[Map[Pos, Int]] =
    starts.map(s => traverse(s, neighbors)
      .view
      .filterKeys(ends.contains)
      .toMap)

given Read[TopographicMap] = TopographicMap(_)

object Day10 extends DayOf2024[TopographicMap](10, "Hoof It"):

  override def part1(sm: TopographicMap): Long =
    sm.trailheads.map(_.keySet.size).sum

  override def part2(sm: TopographicMap): Long =
    sm.trailheads.map(_.values.sum).sum

//Day 10: Hoof It
//  parse : 0.65ms
//  part 1: 197.ms -> 674
//  part 2: 5.24ms -> 1372