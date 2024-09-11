package year2023.day11

import puzzle.Puzzle
import common.Default
import common.Default.default
import parse.{*, given}
import coord.{Pos, given}
import grid.{CharGrid, Grid, MapGrid}
import line.Line

import scala.util.chaining.*

given Default[Char] = '.'

case class Image(g: Grid[Char]):
  private val gt = g.transpose
  private val exr =
    for
      i <- g.gridBox.dl.y to g.gridBox.ur.y
      if g.row(i).forall(_ == '.')
    yield i
  //g.rows.zipWithIndex.collect { case (row, i) if row.forall(_ == '.') => i }
  private val exc =
    for
      i <- g.gridBox.dl.x to g.gridBox.ur.x
      if g.col(i).forall(_ == '.')
    yield i
  //gt.zipWithIndex.collect { case (col, i) if col.forall(_ == '.') => i }

  private val gs =
    g.allPos
  //      g.zipWithIndex
  //        .flatMap((r, y) =>
  //          r.zipWithIndex
  //            .collect { case (c, x) if c == '#' => Pos(x, y) })

  def expandDistances(expandSpace: Int = 1): Long =
    def dist(g1: Pos, g2: Pos): Long =
      val l = Line(g1, g2)
      val cnt = if expandSpace == 1 then 1 else expandSpace - 1
      val md = g1.manhattan(g2).toLong
      val rs = exr.count(l.rangeByCoord(_.y).contains) * cnt
      val cs = exc.count(l.rangeByCoord(_.x).contains) * cnt
      md + cs + rs

    gs.toSeq.combinations(2)
      .map(g => dist(g.head, g.last))
      .sum

given Read[Image] = MapGrid.apply(_).pipe(Image.apply)

object Day11_ extends Puzzle[Image](2023, 11, "Cosmic Expansion"):

  override def prep(input: String): Image =
    MapGrid(input).pipe(Image.apply)
    //input.asStrs.map(_.toVector).toVector.pipe(Image.apply)

  override def part1(galaxies: Image): Long =
    galaxies.expandDistances()

  override def part2(galaxies: Image): Long =
    galaxies.expandDistances(1000000)

//Day 11: Cosmic Expansion
//  prep: 74.6ms
//  part 1: 277.ms - 9445168
//  part 2: 182.ms - 742305960572
