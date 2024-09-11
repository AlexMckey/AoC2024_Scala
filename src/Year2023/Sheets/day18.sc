import AoCLib.exts.*
import AoCLib.dir.Dir
import AoCLib.geometry.Geometry.polygonArea
import AoCLib.pos.{Pos, axisNeighbors, posOrdering}

import scala.util.chaining.*

val s = "R 6 (#70c710)\nD 5 (#0dc571)\nL 2 (#5713f0)\nD 2 (#d2c081)\nR 2 (#59c680)\nD 2 (#411b91)\nL 5 (#8ceee2)\nU 2 (#caa173)\nL 1 (#1b58a2)\nU 2 (#caa171)\nR 2 (#7807d2)\nU 3 (#a77fa3)\nL 2 (#015232)\nU 2 (#7a21e3)"

extension (hex: String)
  def hex2int: Int = Integer.parseInt(hex, 16)

case class Dig(dir: String, cnt: Int, color: String):
  def convertColorToDig: Dig =
    val (coord,d) = color.substring(1).splitAt(5)
    val dir = "RDLU"(d.toInt)
    Dig(dir.toString, coord.hex2int, "")

def dig(start: Pos)(dir: String, cnt: Int): Pos =
  val d = dir match
    case "R" => Dir.E
    case "L" => Dir.W
    case "U" => Dir.N
    case "D" => Dir.S
  start + d.delta * cnt

val cmds = s.asStrs.map {
  case s"$dir $cnt ($color)" => Dig(dir, cnt.toInt, color)
}

def digRoute(cmds: Seq[Dig]): Seq[Pos] =
  cmds.scanLeft(Pos.zero)((p, cmd) => dig(p)(cmd.dir, cmd.cnt)).tail

def calcRouteLen(cmds: Seq[Dig]): Long = cmds.map(_.cnt).sum

def calcArea(cmds: Seq[Dig]): Long =
  val route = digRoute(cmds)
  val routeLen = calcRouteLen(cmds)
  polygonArea(route) + routeLen / 2 + 1

calcArea(cmds)

val newCmds = cmds.map(_.convertColorToDig)
digRoute(newCmds)
calcRouteLen(newCmds)
calcArea(newCmds)