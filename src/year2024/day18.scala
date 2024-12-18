package year2024.day18

import box.Box
import year2024.DayOf2024
import parse.{*, given}
import coord.{Coord, Pos, pos, posRead}
import graph.traverse.BFS
import math.BinarySearch

type Input = List[Pos]

given Read[Input] = Read("\n")
given gb: Box = Box(Pos(70,70))

object Day18 extends DayOf2024[Input](18, "RAM Run"):

  def ns(ps: List[Pos])(p: Pos)(using gb: Box): Iterable[Pos] =
    p.nearAxis.filter(gb.contains).filterNot(ps.contains).toSeq

  override def part1(lps: Input): Long =
    BFS.stepCount(gb.min, gb.max)(ns(lps.take(1024))).getOrElse(0)

  override def part2(lps: Input): String =
    val res = BinarySearch.binarySearch(1025, lps.length)
      (lps.take)
      (ls => BFS.stepCount(gb.min, gb.max)(ns(ls)) == None)
    val pos = res._2.last
    s"${pos.x},${pos.y}"

//Day 18: RAM Run
//  parse : 65.1ms
//  part 1: 185.ms -> 326
//  part 2: 235.ms -> 18,62
