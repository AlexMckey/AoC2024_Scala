package year2024.day13

import year2024.DayOf2024
import parse.{*, given}
import coord.Pos

case class ClawMachine(a: Pos, b: Pos, p: Pos):
  def calcPushButtons(d: Long = 0L): Option[(Long, Long)] =
    val (an, ad) = (b.x * (p.y + d) - b.y * (p.x + d), b.x.toLong * a.y - b.y * a.x)
    val (bn, bd) = (a.x * (p.y + d) - a.y * (p.x + d), a.x.toLong * b.y - a.y * b.x)
    Option.when(bd != 0 && bn % bd == 0 && ad != 0 && an % ad == 0):
      an / ad -> bn / bd

type I = List[ClawMachine]

given Read[Pos] = Read(""".+: X[+|=](\d+), Y[+|=](\d+)""".r)
given Read[ClawMachine] = Read("\n")
given Read[List[ClawMachine]] = Read("\n\n")

object Day13 extends DayOf2024[I](13, "Claw Contraption"):

  extension (l: List[(Long, Long)])
    def cost: Long = l.map((a,b) => a*3 + b).sum

  override def part1(cgs: I): Long =
    cgs.flatMap(_.calcPushButtons()).cost

  override def part2(cgs: I): Long =
    cgs.flatMap(_.calcPushButtons(10000000000000L)).cost

//Day 13: Claw Contraption
//  parse : 43.3ms
//  part 1: 6.72ms -> 28138
//  part 2: 1.10ms -> 108394825772874