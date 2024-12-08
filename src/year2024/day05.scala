package year2024

import year2024.DayOf2024
import parse.{*, given}
import exts.iterables.middle

case class Input(rules: Set[(Int, Int)], prints: List[Vector[Int]]):
  val ordering: Ordering[Int] = Ordering.fromLessThan((l,r) => rules.contains(l -> r))

given Read[Vector[Int]] = Read(",")
given Read[List[Vector[Int]]] = Read("\n")
given Read[(Int,Int)] = Read("""(\d+)\|(\d+)""".r)
given Read[Set[(Int, Int)]] = Read("\n")
given Read[Input] = Read("\n\n")

object Day05 extends DayOf2024[Input](5, "Print Queue"):

  override def part1(i: Input): Int =
    given Ordering[Int] = i.ordering
    i.prints
      .filter(print => print == print.sorted)
      .map(middle)
      .sum

  override def part2(i: Input): Int =
    given Ordering[Int] = i.ordering
    i.prints
      .filterNot(print => print == print.sorted)
      .map(_.sorted)
      .map(middle)
      .sum

//Day 5: Print Queue
//  parse : 63.5ms
//  part 1: 15.7ms -> 6267
//  part 2: 5.25ms -> 5184