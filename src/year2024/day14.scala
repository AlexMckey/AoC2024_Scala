package year2024.day14

import common.Default
import year2024.DayOf2024
import parse.{*, given}
import coord.{Pos, given}
import math.%+
import exts.iterables.groupCount
import grid.MapGrid

extension (p: Pos)
  def adjustInBox(other: Pos): Pos = Pos(p.x %+ other.x, p.y %+ other.y)
  def quadrant(center: Pos): Pos =
    val d = p - center
    Pos(d.x.sign, d.y.sign)

case class Robot(p: Pos, v: Pos):
  def after(time: Int)(using area: Pos): Robot =
    Robot((p + v * time).adjustInBox(area), v)

type I = List[Robot]

given Read[Robot] = Read("""p=(-?\d+,-?\d+) v=(-?\d+,-?\d+)""".r)
given Read[List[Robot]] = Read("\n")

object Day14 extends DayOf2024[I](14, "Restroom Redoubt"):

  given room: Pos = Pos(101,103)
  val pDiv: Pos = room / 2

  extension (robots: List[Robot])
    def safetyFactor: Long =
      robots
        .map(_.p.quadrant(pDiv))
        .groupCount(identity)
        .filterNot(_._1.multiple == 0)
        .values
        .product

    def findEasterEgg: (List[Robot],Int) =
      Iterator.iterate(robots)(_.map(_.after(1)))
        .zipWithIndex
        .find(_._1.groupCount(_.p).values.forall(_ <= 1))
        .get

  override def part1(robots: I): Long =
    robots.map(_.after(100)).safetyFactor

  override def part2(robots: I): Long =
    given Default[Char] = ' '
    val (rs, res) = robots.findEasterEgg
    println(MapGrid(rs.map(_.p -> '*').toMap).toString)
    res

//Day 14: Restroom Redoubt
//  parse : 48.9ms
//  part 1: 46.8ms -> 229421808
//  part 2: 1.21s -> 6577