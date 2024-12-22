package year2024.day19

import year2024.DayOf2024
import parse.{*, given}

import memo.{Cache, Memoize, Memoized}

case class Input(towels: List[String], designs: Array[String]):
  def count(design: String): Long =
    given cache: Cache[Int, Long] = Cache.empty
    def rec(idx: Int): Memoized[Int, Long] =
      Memoize(idx) {
        if idx == design.length then 1L
        else
          towels
            .filter(design.startsWith(_, idx))
            .map(pattern => rec(idx + pattern.size))
            .sum
      }
    rec(0)

  val variations: Array[Long] = designs.map(count)

given Read[Input] = Read.product("\n\n")
given Read[List[String]] = Read.seq(", ")
given Read[Array[String]] = Read.seq("\n")

object Day19 extends DayOf2024[Input](19, "Linen Layout"):

  override def part1(i: Input): Long =
    i.variations.count(_ != 0)

  override def part2(i: Input): Long =
    i.variations.sum

//Day 19: Linen Layout
//  parse : 158.ms
//  part 1: 1.53ms -> 340
//  part 2: 0.70ms -> 717561822679428
