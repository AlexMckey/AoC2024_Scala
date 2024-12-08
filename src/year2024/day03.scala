package year2024.day03

import year2024.DayOf2024
import parse.{*, given}

import scala.annotation.tailrec

type I = List[String] ~ "mul\\((\\d{1,3}),(\\d{1,3})\\)|do\\(\\)|don't\\(\\)"

object Day03 extends DayOf2024[I](3, "Mull It Over"):

  @tailrec
  def process(ms: List[String], acc: Int = 0, isMulOn: Boolean = true): Int =
    if ms.isEmpty then acc
    else ms.head match
      case s"mul($x,$y)" if isMulOn => process(ms.tail, acc + x.toInt * y.toInt, isMulOn)
      case "do()" => process(ms.tail, acc, true)
      case "don't()" => process(ms.tail, acc, false)
      case _ => process(ms.tail, acc, isMulOn)

  override def part1(s: I): Int =
    process(s.filter(_.startsWith("mul")))

  override def part2(s: I): Int =
    process(s)

//Day 3: Mull It Over
//  parse : 29.8ms
//  part 1: 43.9ms -> 170778545
//  part 2: 5.76ms -> 82868252