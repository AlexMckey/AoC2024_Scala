package year2024.day22

import year2024.DayOf2024
import parse.{*, given}

type Input = List[Long]
given Read[Input] = Read.seq("\n")

type Deltas = (Int, Int, Int, Int)

extension (a: Long)
  infix def mix(b: Long): Long = a ^ b
  def prune: Long = a % 16777216L

  def calcNext: Long =
    var n = a
    n = (n mix n << 6).prune
    n = (n mix n >> 5).prune
    n = (n mix n << 11).prune
    n

  def steps(n: Int): Long =
    Iterator
      .iterate(a)(_.calcNext)
      .drop(n)
      .next()

def deltas(l: Seq[Int]): Map[Deltas, Int] =
  def rec(lst: Seq[Int], acc: Map[Deltas, Int]): Map[Deltas, Int] = lst match
    case Nil => acc
    case Seq(a, b, c, d, e, _*) =>
      val ds = (b - a, c - b, d - c, e - d)
      if acc.contains(ds) then rec(lst.tail, acc)
      else rec(lst.tail, acc.updated(ds, e))
    case _ => acc

  rec(l, Map.empty)

object Day22 extends DayOf2024[Input](22, "Monkey Market"):

  override def part1(i: Input): Long =
    i.map(_.steps(2000)).sum

  override def part2(i: Input): Long =
    i.flatMap(x => deltas((1 to 2000)
        .scanLeft(x)((n, _) => n.calcNext)
        .map(_.toInt % 10)))
      .groupMapReduce(_._1)(_._2)(_ + _)
      .values
      .max

//Day 22: Monkey Market
//  parse : 16.3ms
//  part 1: 45.9ms -> 16039090236
//  part 2: 4.01s -> 1808