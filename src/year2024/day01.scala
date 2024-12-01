package year2024.day01

import puzzle.Puzzle
import parse.{*, given}

//import scala.math.abs

type I = List[List[Int] - """\s+"""] - "\n"

object Day01 extends Puzzle[I](2024, 1, "Historian Hysteria"):

  override def part1(prs: I): Int =
    val ids = prs.transpose
    ids.head.sorted
       .zip(ids.last.sorted)
       .map((a, b) => (a - b).abs)
       .sum

  override def part2(prs: I): Int =
    val ids    = prs.transpose
    val idsCnt = ids.last.groupMapReduce(identity)(_ => 1)(_ + _).withDefaultValue(0)
    ids.head
       .map(id => id * idsCnt(id))
       .sum

//Day 1: Historian Hysteria
//  parse : 35.9ms
//  part 1: 14.4ms -> 1151792
//  part 2: 27.3ms -> 21790168