package year2024.day01

import year2024.DayOf2024
import parse.{*, given}
import exts.iterables.countItems

type I = List[List[Int]]

given Read[List[Int]] = Read("\\s+")
given Read[I] = Read("\n")

object Day01 extends DayOf2024[I](1, "Historian Hysteria"):

  override def part1(prs: I): Int =
    val List(ids1, ids2) = prs.transpose
    ids1.sorted
       .zip(ids2.sorted)
       .map((a, b) => (a - b).abs)
       .sum

  override def part2(prs: I): Int =
    val List(ids1, ids2) = prs.transpose
    val idsCnt = ids2.countItems
    ids1
      .map(id => id * idsCnt(id))
      .sum

//Day 1: Historian Hysteria
//  parse : 35.9ms
//  part 1: 14.4ms -> 1151792
//  part 2: 27.3ms -> 21790168