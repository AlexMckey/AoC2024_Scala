package year2023.day13

import year2023.DayOf2023
import common.Default
import common.Default.default
import parse.{*, given}
import coord.{Coord, Pos, given}
import grid.{CharGrid, Grid, MapGrid}

type Image = List[String]
type DataType = List[Image]

given Read[Image] = Read.seq("\n")
given Read[DataType] = Read.seq("\n\n")

object Day13 extends DayOf2023[DataType](13, "Point of Incidence"):

//  override def prep(input: String): DataType =
//    input.splitByBlankLines.map(_.asStrs)

  def findMirrors(m: List[String], check: Int => Boolean): Int =
    def findIndex(m: List[String]): Option[Int] =
      def splitAndCheck(at: Int): Int =
        m.map(s => s.substring(0, at)
              .reverse
              .zip(s.substring(at))
              .count(_ != _))
          .sum

      (1 until m.head.length)
        .map(splitAndCheck)
        .zipWithIndex
        .collectFirst { case (cnt, i) if check(cnt) => i + 1 }

    val vi = findIndex(m)
    val hi = findIndex(m.transpose.map(_.mkString))
    vi.getOrElse(hi.map(_ * 100).get)

  override def part1(mirrors: DataType): Long =
    mirrors.map(findMirrors(_, _ == 0)).sum

  override def part2(mirrors: DataType): Long =
    mirrors.map(findMirrors(_, _ == 1)).sum

//Day 13: Point of Incidence
//  prep: 60.3ms
//  part 1: 179.ms - 40006
//  part 2: 48.0ms - 28627