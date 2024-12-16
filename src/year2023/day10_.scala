package year2023.day10

import year2023.DayOf2023
import common.Default
import common.Default.default
import parse.{*, given}
import exts.*
import coord.{Dir, Neighbors, Pos, given}
import Dir.*
import grid.{CharGrid, Grid, VectorGrid}
import geometry.Geometry.*
import VectorGrid.charVectorGridReader
import graph.traverse.old.DFS

case class Path(g: CharGrid):
  given Default[Char] = '.'

  private val pipes = Map(
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

  val path: Seq[Pos] =
    val start = g.find('S').getOrElse(Pos.zero)
    val startNears = start.nearAxis.filter(n => nearPipes(n, g(n)).contains(start)).map(_ - start).toSet
    val startChar = pipes.map(_.swap)(startNears.map(asDir))
    val newG = g.updated(start, startChar)
    given Neighbors[Char] with
      override def neighbors(p: Pos, a: Char): Iterator[Pos] = nearPipes(p, a)
    newG.dfs(start)._2

given Read[Path] = (s: String) => Path(VectorGrid.apply(s))

object Day10_ extends DayOf2023[Path](10, "Pipe Maze"):

  override def part1(inputData: Path): Int =
    inputData.path.length / 2

  override def part2(inputData: Path): Long =
    polygonArea(inputData.path) - inputData.path.length / 2 + 1

//Day 10: Pipe Maze
//  parse : 644.ms
//  part 1: 0.84ms -> 7102
//  part 2: 23.2ms -> 363
