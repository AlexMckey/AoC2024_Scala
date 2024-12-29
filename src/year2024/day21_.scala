package year2024.day21_

import year2024.DayOf2024
import parse.{*, given}
import grid.{CharGrid, MapGrid}
import coord.{Dir, Pos, pos}
import Dir.*
import coord.GridDir.asDirChar
import memo.{Cache, Memoize, Memoized}
import graph.traverse.DFS

type Nums = List[String]
given Read[Nums] = Read.seq("\n")

object Day21_ extends DayOf2024[Nums](21, "Keypad Conundrum"):

  val NumPad: MapGrid[Char] = MapGrid("789\n456\n123\n 0A")
  val DirPad: MapGrid[Char] = MapGrid(" ^A\n<v>")

  extension (code: String)
    def complexity(robots: Int): Long =
      solve(robots + 1, code) * code.init.toLong

  def solve(depth: Int, code: String): Long =
    given Cache[(Int, List[Char]), Long] = Cache.empty

    def presses(path: List[Pos]): List[Char] =
      if path.length < 2 then List('A')
      else
        path.sliding(2)
          .map { ar => ar(0).between(ar(1)).asDirChar }
          .toList.appended('A')

    extension (g: CharGrid)
      def shortestPaths(from: Pos, to: Pos): Iterator[List[Pos]] =
        def neighbors(p: Pos): Seq[Pos] =
          p.nearAxis
            .filter(g.contains)
            .filterNot(g(_) == ' ')
            .toSeq

        val paths = DFS.search(from, to)(neighbors)
        val minSize = paths.map(_.size).min
        paths.filter(_.size == minSize).map(_.toList).iterator

    def pressCount(stage: Int, code: List[Char]): Memoized[(Int, List[Char]), Long] =
      val pad = if stage == 0 then NumPad else DirPad
      if stage == depth then code.size
      else
        ('A' :: code)
          .map(pad.posOf(_).get)
          .sliding(2)
          .map { ar =>
            pad
              .shortestPaths(ar(0), ar(1))
              .map(presses)
              .map(code => Memoize(stage, code)(pressCount(stage + 1, code)))
              .min
          }
          .sum

    pressCount(0, code.toCharArray.toList)

  override def part1(nums: Nums): Long =
    nums.map(_.complexity(2)).sum

  override def part2(nums: Nums): Long =
    nums.map(_.complexity(25)).sum

//Day 21: Keypad Conundrum
//  parse : 3.55ms
//  part 1: 139.ms -> 217662
//  part 2: 280.ms -> 263617786809000
