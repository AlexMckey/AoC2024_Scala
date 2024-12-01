package year2023.day10

import year2023.DayOf2023
import common.Default
import common.Default.default
import parse.{*, given}
import exts.*
import coord.{Dir, Neighbor, Pos, given}
import Dir.*
import grid.{CharGrid, Grid, VectorGrid}
import VectorGrid.charVectorGridReader

object Day10 extends DayOf2023[CharGrid](10, "Pipe Maze"):

  given Default[Char] = '.'

  private val defChar = default[Char]

  override def prep(input: String): CharGrid =
    val g = VectorGrid(input)
    val start      = g.find('S').getOrElse(Pos.zero)
    val startNears = start.nearAxis.filter(n => nearPipes(n, g(n)).contains(start)).map(_ - start).toSet
    val startChar  = pipes.map(_.swap)(startNears.map(asDir))
    val newG       = g.updated(start, startChar)
    given Neighbor[Char] with
      override def neighbors(p: Pos, a: Char): Iterator[Pos] = nearPipes(p, a)
    val path = newG.dfs(start)._1
    VectorGrid(path.map((p,_) => p -> newG(p)))

  private val pipes: Map[Char, Set[Dir]] = Map(
    '.' -> Set.empty,
    '-' -> Set(W, E),
    '|' -> Set(S, N),
    'F' -> Set(S, E),
    'L' -> Set(E, N),
    'J' -> Set(N, W),
    '7' -> Set(W, S)
  )

  private def nearPipes(p: Pos, a: Char): Iterator[Pos] =
    (pipes(a).map(p.toDir) - p).iterator

  private def isInside(inside: Boolean, c: Char): Boolean = c match
    case '|' | 'F' | '7' => !inside
    case _ => inside

  override def part1(path: CharGrid): Int =
    path.count(_ != defChar) / 2

  override def part2(path: CharGrid): Int =
    path.rows
      .flatMap(_.scanLeft(false -> false)
        { case ((_, status), char) => (char == defChar) -> isInside(status, char) }
      .map(_ && _))
      .count(identity)

//Day 10: Pipe Maze
//  parse : 631.ms
//  part 1: 5.75ms -> 7102
//  part 2: 12.1ms -> 363
