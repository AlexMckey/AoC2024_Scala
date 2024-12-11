package year2024.day08

import year2024.DayOf2024
import parse.{*, given}
import coord.{Pos, given}
import grid.MapGrid
import grid.CharGrid
import grid.MapGrid.charMapGridReader

type I = CharGrid

object Day08 extends DayOf2024[I](8, "Resonant Collinearity"):

  def twoAntinodes(a1: Pos, a2: Pos)(using g: CharGrid): Set[Pos] =
    val d = a2 - a1
    Set(a1 - d, a2 + d).filter(g.contains)

  def manyAntinodes(a1: Pos, a2: Pos)(using g: CharGrid): Set[Pos] =
    val d = a2 - a1
    Iterator.iterate(a1)(_ - d).takeWhile(g.contains).toSet ++
      Iterator.iterate(a2)(_ + d).takeWhile(g.contains).toSet

  def part(antennas: I, findAntinodes: (Pos, Pos) => Set[Pos]): Int =
    val ms = antennas
      .filter(_ != '.')
      .iterator
      .toSet
      .groupMap(_._2)(_._1)
    (for
      (_, ps) <- ms
      a1 <- ps
      a2 <- ps
      if a1 != a2
      as <- findAntinodes(a1, a2)
    yield as)
      .toSet
      .size

  override def part1(antennas: I): Int =
    given CharGrid = antennas
    part(antennas, twoAntinodes)

  override def part2(antennas: I): Long =
    given CharGrid = antennas
    part(antennas, manyAntinodes)

//Day 8: Resonant Collinearity
//  parse : 63.2ms
//  part 1: 67.4ms -> 261
//  part 2: 33.8ms -> 898