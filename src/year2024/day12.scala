package year2024.day12

import year2024.DayOf2024
import parse.{*, given}
import grid.{CharGrid, MapGrid}
import coord.{Coord, Dir, Pos, pos}
import box.Box
import traverse.BFS

type Region = Set[Pos]
type I = Set[Region]

given Read[Set[Region]] with
  override def read(input: String): Set[Region] =
    def neighbors(g: CharGrid)(p: Pos): Iterator[Pos] =
      p.nearAxis.filter(g.contains).filter(g(_) == g(p))
    val g = MapGrid(input)
    BFS.components(g.allPos, neighbors(g))

object Day12 extends DayOf2024[I](12, "Garden Groups"):

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

  override def part1(garden: I): Long =
    garden.toList.map(r => r.size * perimeter(r)).sum

  override def part2(garden: I): Long =
    garden.toList.map(r => r.size * sides(r)).sum

//Day 12: Garden Groups
//  parse : 329.ms
//  part 1: 47.7ms -> 1377008
//  part 2: 130.ms -> 815788