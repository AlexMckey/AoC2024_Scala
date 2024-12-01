package year2023.day01

import puzzle.Puzzle
import parse.{*, given}

type I = List[String] - "\n"

object Day01 extends Puzzle[I](2023, 1, "Trebuchet?!"):
  
  def part(strs: I, pattern: String): Int =
    val numsL = pattern.split('|')
    strs.map(x =>
        val l1 = pattern.r.findFirstMatchIn(x).get.group(0)
        val l2 = pattern.reverse.r.findFirstMatchIn(x.reverse).get.group(0)
        val a = numsL.indexOf(l1) % 10
        val b = numsL.indexOf(l2.reverse) % 10
        s"$a$b".toInt)
      .sum

  override def part1(strs: I): Int =
    val nums = "0|1|2|3|4|5|6|7|8|9"
    part(strs, nums)

  override def part2(strs: I): Int =
    val nums = "0|1|2|3|4|5|6|7|8|9|zero|one|two|three|four|five|six|seven|eight|nine"
    part(strs, nums)

//Day 1: Trebuchet?!
//  prep: 5.94ms
//  part 1: 43.6ms - 54953
//  part 2: 14.2ms - 53868