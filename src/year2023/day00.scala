package year2023.day00

import year2023.DayOf2023
import parse.given

object day00 extends DayOf2023[String] (0, "Sample"):

  override def part1(chars: String): Int =
    // Part 1 is fast.
    Thread.sleep(50)
    chars.length

  override def part2(chars: String): Int =
    // Part 2 is kinda slow.
    Thread.sleep(500)
    chars.count(_.isLetterOrDigit)
