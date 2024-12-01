package year2023.day14

import year2023.DayOf2023
import common.Default
import parse.{*, given}
import coord.{Coord, Dir, Pos, given}
import grid.{CharGrid, Grid, VectorGrid}
import cycle.*
import exts.*
//import grid.VectorGrid.charVectorGridReader

import scala.util.chaining.*
import scala.util.matching.Regex

//given Default[Char] = '.'

given Read[CharGrid] = VectorGrid.apply(_)

object Day14_ extends DayOf2023[CharGrid](14, "Parabolic Reflector Dish"):

//  type Dish = Grid[Char]
//  type DataType = Dish

//  override def prep(input: String): DataType = VectorGrid(input)

  val rg: Regex = "(#*)?([O.]+)(#*)?".r
  val sorts = "O."

  def fall(s: String): String =
    rg.findAllMatchIn(s)
      .map(_.subgroups match
        case List(f, snd, e) =>
          s"$f${snd.sorted(Ordering.by[Char, Int](sorts.indexOf(_)))}$e"
        case _ => "") //такого не может быть
      .mkString

  extension (g: CharGrid)
    def calcNorthLoad: Int =
      g.rows.zipWithIndex
        .map((r,i) => (g.gridBox.ur.y - i + 1) * r.count(_ == 'O'))
        .sum

    def tilt: CharGrid =
      g.cols
        .map(_.mkString)
        .map(fall)
        .mkString("\n")
        .pipe(VectorGrid.apply)
        .transpose

  def cycleTilt(g: CharGrid): CharGrid =
    4.repeated(g)(_.tilt.rotateCW().toVectorGrid)

  override def part1(reflector: CharGrid): Int =
    reflector.tilt.calcNorthLoad

  override def part2(reflector: CharGrid): Int =
    val is = Iterator.iterate(reflector)(cycleTilt)
    val Some((init,cycle)) = detectCycle(is,0): @unchecked
    val cnt = cycledEquivalentIterations(init, cycle, 1000000000).toInt - init
    is.drop(cnt - 1).next.calcNorthLoad
    
//Day 14: Parabolic Reflector Dish
//  prep: 83.4ms
//  part 1: 435.ms - 108813
//  part 2: 28.3s - 104533