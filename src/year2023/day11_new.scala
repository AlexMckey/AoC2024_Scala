package year2023.day11

import year2023.DayOf2023
import common.Default
import common.Default.default
import parse.{*, given}
import coord.{Pos, given}
import grid.{CharGrid, Grid, MapGrid}

//given Default[Char] = '.'

given Read[CharGrid] with
  override def read(input: String): CharGrid = MapGrid.apply(input)

object Day11_new extends DayOf2023[CharGrid](11, "Cosmic Expansion"):

  private def expandAxis(k: Int = 2)(g: CharGrid): CharGrid =
    val colsIdx = g.cols.zipWithIndex.collect{ case (c, i) if c.forall(_ == default[Char]) => i }.toSeq
    val rowsIdx = g.rows.zipWithIndex.collect { case (r, i) if r.forall(_ == default[Char]) => i }.toSeq
    val r = g.asInstanceOf[grid.MapGrid[Char]].debug
    g.mapToPos((p, ch) => Pos(p.x + colsIdx.count(_ < p.x) * (k - 1),
                              p.y + rowsIdx.count(_ < p.y) * (k - 1)) -> ch)

  private def calcDistances(g: CharGrid): Long =
    g.allPos.toSeq
      .combinations(2)
      .map(g => g.head manhattan g.last)
      .map(_.toLong)
      .sum

  override def part1(galaxies: CharGrid): Long =
    expandAxis().andThen(calcDistances)(galaxies)

  override def part2(galaxies: CharGrid): Long =
    expandAxis(1000000).andThen(calcDistances)(galaxies)

//Day 11: Cosmic Expansion
//  prep: 74.6ms
//  part 1: 277.ms - 9445168
//  part 2: 182.ms - 742305960572
