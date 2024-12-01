package year2023.day11

import box.Box
import year2023.DayOf2023
import common.Default
import common.Default.default
import parse.{*, given}
import coord.{Coord, Pos, given}
import grid.{CharGrid, Grid, MapGrid}
import line.Line

import scala.util.chaining.*

case class Galaxies(g: Set[Pos]):
  val b: (Pos, Pos) = Coord.boundingBox(g)
  private val minG = b._1
  private val maxG = b._2

  def toExpand(m: Set[Pos], f: Pos => Int): Seq[Int] =
    for
      i <- f(minG) to f(maxG)
      if !m.exists(p => f(p) == i)
    yield i

  private val rowToExpand = toExpand(g, _.y)
  private val colToExpand = toExpand(g, _.x)

  def dist(g1: Pos, g2: Pos, k: Int = 1): Long =
    val l = Line(g1, g2)
    val cnt = if k == 1 then 1 else k - 1
    val md = g1.manhattan(g2).toLong
    val rs = rowToExpand.count(l.rangeByCoord(_.y).contains) * cnt
    val cs = colToExpand.count(l.rangeByCoord(_.x).contains) * cnt
    md + cs + rs

  def expandDistances(expandSpace: Int = 1): Long =
    g.toSeq.combinations(2)
      .map(g => dist(g.head, g.last, expandSpace))
      .sum

given Read[Galaxies] = MapGrid.apply(_).allPos.pipe(Galaxies.apply)

object Day11 extends DayOf2023[Galaxies](11, "Cosmic Expansion"):

  override def part1(galaxies: Galaxies): Long =
    galaxies.expandDistances()

  override def part2(galaxies: Galaxies): Long =
    galaxies.expandDistances(1000000)

//Day 11: Cosmic Expansion
//  prep: 74.6ms
//  part 1: 277.ms - 9445168
//  part 2: 182.ms - 742305960572