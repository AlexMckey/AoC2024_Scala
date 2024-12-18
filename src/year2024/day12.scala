package year2024.day12

import year2024.DayOf2024
import parse.{*, given}
import grid.{CharGrid, MapGrid}
import coord.{Coord, Dir, Pos, pos}
import box.Box
import graph.traverse.old.BFS

type Region = Set[Pos]

case class Garden(s: String):
  def neighbors(g: CharGrid)(p: Pos): Iterator[Pos] =
    p.nearAxis.filter(g.contains).filter(g(_) == g(p))
  lazy val g: CharGrid = MapGrid(s)
  lazy val regions: List[Region] = BFS.components(g.allPos, neighbors(g)).toList

given Read[Garden] = Garden(_)

object Day12 extends DayOf2024[Garden](12, "Garden Groups"):

  def perimeter(region: Set[Pos]): Int =
    region
      .iterator
      .map(pos => 4 - pos.nearAxis.count(region))
      .sum

  def sides(region: Set[Pos]): Int =
    import Dir.*
    val (p1, p2) = Coord.boundingBox(region)
    val b = Box(p1.toDir(NW), p2)
    val ps = b.iterator.map(p => List(p, p.toDir(S), p.toDir(E), p.toDir(SE)))
    ps.map(_.map (p => if region.contains(p) then 1 else 0 )
      .zip(List(1, -1, -1, 1))
      .map(_ * _)
      .sum.abs)
      .sum

  override def part1(garden: Garden): Long =
    garden.regions.map(r => r.size * perimeter(r)).sum

  override def part2(garden: Garden): Long =
    garden.regions.map(r => r.size * sides(r)).sum

//Day 12: Garden Groups
//  parse : 329.ms
//  part 1: 47.7ms -> 1377008
//  part 2: 130.ms -> 815788