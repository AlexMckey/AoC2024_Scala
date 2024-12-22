package year2024.day06

import year2024.DayOf2024
import parse.{*, given}
import grid.*
import MapGrid.charMapGridReader
import coord.Dir
import walker.{Walker, Travel}

type I = CharGrid

case class GuardTravel(grid: CharGrid) extends Travel(grid):
  override def start: Walker = Walker(grid.posOf('^').get, Dir.N)
  override def stopWalk(st: Walker): Boolean = grid.get(st.p).isEmpty
  override def isBlocked(st: Walker): Boolean = grid.isEq(st.p, '#')
  override def changeStateRule(st: Walker): Walker =
    val fwd = st.step
    if isBlocked(fwd)
    then st.turnRight
    else fwd

object Day06 extends DayOf2024[I](6, "Guard Gallivant"):

  override def part1(g: I): Int =
    val travel = GuardTravel(g)
    travel.traverse._1.size

  override def part2(g: I): Int =
    val travel = GuardTravel(g)
    val path = travel.traverse._1 - travel.start.p
    val res = path.toList.map{ p =>
        val newG = g.updated(p, '#')
        val status = GuardTravel(newG).traverse._2
        p -> status
    }
    res.count(_._2)

//Day 6: Guard Gallivant
//  parse : 84.7ms
//  part 1: 66.3ms -> 4580
//  part 2: 8.66s -> 1480