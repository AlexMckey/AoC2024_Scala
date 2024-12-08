package year2023.day06

import year2023.DayOf2023
import parse.{*, given}
import exts.*

import scala.annotation.tailrec

case class Num(n: Long)

case class Pars(ls: List[Num])

type I = List[Pars]

given Read[Num] = Read("""(?:(\d+)\s*)""".r)
given Read[List[Num]] = Read(" ")
given Read[Pars] = Read("""\S+:\s*(.*)""".r)
given Read[I] = Read("\n")

object Day06 extends DayOf2023[I](6, "Wait For It"):

  private def dist(t: Long, maxT: Long): Long = t * (maxT - t)

  private def isWin(r: Seq[Long], t: Int): Boolean = dist(t, r.head) > r.last

  private def countWins(r: Seq[Long]): Long =
    Iterator.from(1)
      .dropWhile(!isWin(r,_))
      .takeWhile(isWin(r,_))
      .size

  override def part1(races: I): Long =
    races.map(_.ls.map(_.n)).transpose.map(countWins).product

  override def part2(races: I): Long =
    val pair = races.map(_.ls.map(_.n)).map(_.mkString("").toLong)
    countWins(pair)

//Day 6: Wait For It
//  prep: 41.6ms
//  part 1: 14.4ms - 138915
//  part 2: 481.ms - 27340847
