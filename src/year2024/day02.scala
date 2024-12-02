package year2024.day02

import year2024.DayOf2024
import parse.{*, given}

import scala.annotation.tailrec

type I = List[List[Int] - " "] - "\n"

object Day02 extends DayOf2024[I](2, "Red-Nosed Reports"):

  def skipOneBad(l: List[Int]): Seq[List[Int]] =
    l.indices.map { i =>
      val (ll, lr) = l.splitAt(i)
      ll ++ lr.tail
    }

  def isSafe(l: List[Int]): Boolean =
    val goalSign = (l.head - l.last).sign
    val sl = l.sliding(2).map { case List(a, b) => a - b }.toList
    sl.forall(d => (d.sign == goalSign) && (1 to 3).contains(d.abs))

  override def part1(reps: I): Int =
    reps.count(isSafe)

  override def part2(reps: I): Int =
    reps.count(l => isSafe(l) || skipOneBad(l).exists(isSafe))

//Day 2: Red-Nosed Reports
//  parse : 59.6ms
//  part 1: 68.6ms -> 432
//  part 2: 71.2ms -> 488