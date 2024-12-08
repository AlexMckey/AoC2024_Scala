package year2023.day18

import year2023.DayOf2023
import common.Default
import parse.{*, given}
import coord.{Dir, Pos, given}
import grid.{CharGrid, VectorGrid}
import geometry.Geometry.*

import scala.annotation.tailrec

extension (hex: String) def hex2int: Int = Integer.parseInt(hex, 16)

case class Dig(dir: String, cnt: Int, color: String):
  def convertColorToDig: Dig =
    val (coord, d) = color.splitAt(5)
    val dir = "RDLU"(d.toInt)
    Dig(dir.toString, coord.hex2int, "")
    
type DataType = List[Dig]

given Read[Dig] = Read("""([RLDU]) (\d+) \(#([\dabcdef]+)\)""")
given Read[DataType] = Read("\n")

object Day18 extends DayOf2023[DataType](18, "Lavaduct Lagoon"):

  def dig(start: Pos)(dir: String, cnt: Int): Pos =
    val d = dir match
      case "R" => Dir.E
      case "L" => Dir.W
      case "U" => Dir.N
      case "D" => Dir.S
    start + d.delta * cnt

  def digRoute(cmds: List[Dig]): Seq[Pos] =
    cmds.scanLeft(Pos.zero)((p, cmd) => dig(p)(cmd.dir, cmd.cnt)).tail

  def calcRouteLen(cmds: List[Dig]): Long = cmds.map(_.cnt.toLong).sum

  def calcArea(cmds: List[Dig]): Long =
    val route    = digRoute(cmds)
    val routeLen = calcRouteLen(cmds)
    val area     = polygonArea(route)
    area + routeLen / 2 + 1

  override def part1(digs: DataType): Long =
    calcArea(digs)

  override def part2(digs: DataType): Long =
    calcArea(digs.map(_.convertColorToDig))

//Day 18: Lavaduct Lagoon
//  prep: 84.9ms
//  part 1: 54.9ms - 68115
//  part 2: 3.15ms - 71262565063800
