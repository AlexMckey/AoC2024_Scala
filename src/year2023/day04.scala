package year2023.day04

import year2023.DayOf2023
import parse.{*, given}

case class Card(id: Int, you: List[Int], win: List[Int]):
  def winSize: Int = win.intersect(you).size

type I = List[Card]

given Read[List[Int]] = Read.seq("\\s+")
given Read[Card] = Read.product("""Card\s+(.*): (.*) \| (.*)""".r)
given Read[I] = Read.seq("\n")

object Day04 extends DayOf2023[I](4, "Scratchcards"):

  override def part1(cards: I): Int =
    val ws = cards
      .map(_.winSize)
    val res = ws.map(c => if c == 0 then 0 else 1 << (c - 1))
    res.sum

  override def part2(cards: I): Int =
    val cache = Array.fill(cards.size)(1)
    cards
      .map(_.winSize)
      .zipWithIndex.foreach((cnt, idx) =>
      if cnt > 0 then (1 to cnt)
        .foreach(i => cache(i + idx) += cache(idx)))
    cache.sum

//Day 4: Scratchcards
//  prep: 110.ms
//  part 1: 1.61ms - 27845
//  part 2: 8.43ms - 9496801
