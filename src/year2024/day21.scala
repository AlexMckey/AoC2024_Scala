package year2024.day21

import year2024.DayOf2024
import parse.{*, given}
import memo.{Cache, Memoize, Memoized}
import graph.*
import grid.MapGrid
import coord.{Dir, GridDir}

type Nums = List[String]
given Read[Nums] = Read.seq("\n")

object Day21 extends DayOf2024[Nums](21, "Keypad Conundrum"):

  import grid.CharGrid

  val NumPad: MapGrid[Char] = MapGrid("789\n456\n123\n 0A")
  val DirPad: MapGrid[Char] = MapGrid(" ^A\n<v>")

  import Dir.*
  import GridDir.asDirChar

  extension (d: Dir)
    def toDirChar: Char =
      d match
        case E => '>'
        case W => '<'
        case N => '^'
        case S => 'v'
        case _ => '.'

  private def toGraph(g: CharGrid): Graph[Char,Char] =
    val edges: List[(Char, Char, Char)] =
      g.iteratorAll
        .filterNot(_._2 == ' ')
        .flatMap { (p, ch) =>
          Dir.axisDirs.flatMap { d =>
            val newP = p.toDir(d)
            if g.contains(newP) && g(newP) != ' '
            then List((ch, g(newP),d.asDirChar))
            else List.empty
          }
        }.toList
    Graph.fromEdges(edges.map(Edge.apply))

  private val numGraph: Graph[Char, Char] = toGraph(NumPad)
  private val dirGraph: Graph[Char, Char] = toGraph(DirPad)

  extension (code: String)
    def complexity(robots: Int): Long =
      solve(robots + 1, code) * code.init.toLong

  def solve(depth: Int, code: String): Long =
    given Cache[(Int, List[Char]), Long] = Cache.empty

    def presses(edges: List[Edge[Char, Char]]): List[Char] =
      edges.map(_.label).prepended('A').reverse

    def pressCount(stage: Int, code: List[Char]): Memoized[(Int, List[Char]), Long] =
      val pad = if stage == 0 then numGraph else dirGraph
      if stage == depth then code.size
      else
        ('A' :: code)
          .sliding(2)
          .map(segment => pad
            .shortestPaths(segment(0), segment(1))
            .map(presses)
            .map(code => Memoize(stage, code)(pressCount(stage + 1, code)))
            .min)
          .sum

    pressCount(0, code.toCharArray.toList)

  override def part1(nums: Nums): Long =
    nums.map(_.complexity(2)).sum

  override def part2(nums: Nums): Long =
    nums.map(_.complexity(25)).sum

//Day 21: Keypad Conundrum
//  parse : 8.06ms
//  part 1: 90.7ms -> 217662
//  part 2: 52.2ms -> 263617786809000
