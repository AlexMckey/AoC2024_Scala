package year2023.day11

import year2023.DayOf2023
import common.Default
import common.Default.default
import parse.{*, given}
import exts.*
import coord.{Dir, Neighbor, Pos, given}
import grid.{CharGrid, Grid, MapGrid}

object Day11__ extends DayOf2023[CharGrid](11, "Cosmic Expansion"):

  given Default[Char] = '.'

  private def expand(expandSpace: Int = 2)(g: CharGrid): Seq[Pos] =
    def expandAxis(axis: Pos => Int): Seq[Int] =
      (0 to axis(g.gridBox.ur)).scanLeft(0) { case (idx, i) =>
        if g.allPos.exists(axis.andThen(_ == i))
        then idx + 1
        else idx + expandSpace
      }
    val rows = expandAxis(_.y)
    val cols = expandAxis(_.x)
    g.allPos.map(p => Pos(cols(p.x), rows(p.y))).toSeq

  private def calcDistances(g: Seq[Pos]): Long =
    val gbx = g.map(_.x).max
    val gby = g.map(_.y).max
    g.combinations(2)
      .map(g => g.head manhattan g.last)
      .map(_.toLong)
      .sum

  override def part1(galaxies: CharGrid): Long =
    expand().andThen(calcDistances)(galaxies)

  override def part2(galaxies: CharGrid): Long =
    expand(1000000).andThen(calcDistances)(galaxies)

//Day 11: Cosmic Expansion
//  prep: 74.6ms
//  part 1: 277.ms - 9445168
//  part 2: 182.ms - 742305960572
