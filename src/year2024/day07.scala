package year2024.day07

import year2024.DayOf2024
import parse.{*, given}

val simpleOps: List[(Long, Long) => Long] = List(_ + _, _ * _)
val concat: (Long, Long) => Long = (a, b) => s"$a$b".toLong

case class Equation(res: Long, args: List[Long]):
  def check(ops: List[(Long, Long) => Long]): Boolean =
    def rec(acc: Long, args: List[Long]): Boolean =
      if args.isEmpty then
        acc == res
      else
        ops.exists(op => rec(op(acc, args.head), args.tail))

    rec(args.head, args.tail)

given operandList: Read[List[Long]] = Read(" ")
given Read[Equation] = Read(": ")
given Read[List[Equation]] = Read("\n")

def concat(lhs: Long, rhs: Long): Long =
  (lhs.toString + rhs.toString).toLong

object Day07 extends DayOf2024[List[Equation]](7, "Guard Gallivant"):

  override def part1(eqs: List[Equation]): Long =
    eqs
      .filter(_.check(simpleOps))
      .map(_.res)
      .sum

  override def part2(eqs: List[Equation]): Long =
    eqs
      .filter(_.check(simpleOps :+ concat))
      .map(_.res)
      .sum

//Day 7: Guard Gallivant
//  parse : 39.4ms
//  part 1: 17.7ms -> 1298103531759
//  part 2: 486.ms -> 140575048428831