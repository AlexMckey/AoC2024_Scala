package year2024.day11

import year2024.DayOf2024
import parse.{*, given}

import exts.iterators.groupMapReduce
import exts.iterables.groupCount

type I = List[Long]

given Read[List[Long]] = Read.seq("\\s")

object Day11 extends DayOf2024[I](11, "Plutonian Pebbles"):

  def blink(stone: Long): List[Long] =
    if stone == 0
    then List(1)
    else
      val s = stone.toString
      val len = s.length
      if len % 2 == 0 then
        val (l, r) = s.splitAt(len / 2)
        List(l.toLong, r.toLong)
      else
        List(stone * 2024)

  def blinkWithCount(stones: Map[Long, Long]): Map[Long, Long] =
    (for
      (stone, cnt) <- stones.iterator
      blinked <- blink(stone)
    yield blinked -> cnt)
      .groupMapReduce(_._1)(_._2)(_ + _)

  def countBlinkedStones(stones: List[Long], blinkCnt: Int): Long =
    val start = stones.groupCount(identity).view.mapValues(_.toLong).toMap
    val res = Iterator.iterate(start)(blinkWithCount).drop(blinkCnt).next()
    res.values.sum

  override def part1(stones: I): Long =
    countBlinkedStones(stones, 25)

  override def part2(stones: I): Long =
    countBlinkedStones(stones, 75)

//Day 11: Plutonian Pebbles
//  parse : 13.2ms
//  part 1: 84.0ms -> 198075
//  part 2: 263.ms -> 235571309320764