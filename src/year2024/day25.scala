package year2024.day25

import year2024.DayOf2024
import parse.{*, given}

case class KeyLock(s: String):
  val isLock: Boolean = s.head == '#'
  val pins: Array[Int] = s.split("\n")
    .map(_.toCharArray)
    .transpose
    .map(_.count(_ == '#')-1)

  def isFit(other: KeyLock): Boolean =
    if other.isLock == isLock then false
    else pins.zip(other.pins).map(_ + _).forall(_ <= 5)

given Read[KeyLock] = KeyLock(_)
given Read[List[KeyLock]] = Read.seq("\n\n")

object Day25 extends DayOf2024[List[KeyLock]](25, "Code Chronicle"):

  override def part1(kls: List[KeyLock]): Int =
    val (ls, ks) = kls.partition(_.isLock)
    ls.flatMap(l => ks.map(k => l.isFit(k)))
      .count(identity)

  override def part2(kls: List[KeyLock]): String =
    "Congratulation!!!"

//Day 25: Code Chronicle
//  parse : 30.4ms
//  part 1: 56.9ms -> 3307
//  part 2: 0.00ms -> Congratulation!!!