package year2024.day00

import puzzle.Puzzle
import parse.given

object Day00 extends Puzzle[String] (2024, 0, "Sample"):

  override def part1(chars: String): Int =
    // Part 1 is fast.
    Thread.sleep(50)
    chars.length

  override def part2(chars: String): Int =
    // Part 2 is kinda slow.
    Thread.sleep(500)
    chars.count(_.isLetterOrDigit)
