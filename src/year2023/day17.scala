package year2023.day17

import year2023.DayOf2023
import common.Default
import parse.{*, given}
import coord.{Dir, Pos, given}
import grid.{CharGrid, VectorGrid}
import traverse.Dijkstra

import scala.annotation.tailrec

given Default[Char] = '.'

given Read[CharGrid] = VectorGrid.apply(_)

object Day17 extends DayOf2023[CharGrid](17, "Clumsy Crucible"):

  extension (c: Char) def heatLoss: Int = c.asDigit

  case class State(p: Pos, curDir: Dir = Dir.E, prevDirCnt: Int = 0)

  def neighbors(canMove: (State, Dir) => Boolean)(st: State)(using g: CharGrid): Set[(State, Int)] =
    (Dir.values.toSet - st.curDir.flip)
      .map(d => st.p.toDir(d) -> d)
      .filter((p, d) => g.gridBox.contains(p) && canMove(st, d))
      .map((p, d) => State(p, d, if d == st.curDir then st.prevDirCnt + 1 else 1) -> g(p).heatLoss)

  def part(g: CharGrid, canMove: (State, Dir) => Boolean, canStop: State => Boolean): Int =
    given CharGrid = g
    val goal   = g.gridBox.max
    val start  = State(g.gridBox.min)
    Dijkstra.search[State](start, st => st.p == goal && canStop(st), neighbors(canMove)).get._2

  override def part1(cityMap: CharGrid): Int =
    def canMove(st: State, d: Dir): Boolean = st.prevDirCnt < 3 || d != st.curDir
    part(cityMap, canMove, _ => true)

  override def part2(cityMap: CharGrid): Int =
    def canMove(st: State, d: Dir): Boolean =
      (st.curDir == d && st.prevDirCnt < 10) || (st.curDir != d && st.prevDirCnt >= 4)
    part(cityMap, canMove, st => st.prevDirCnt >= 4)

//Day 17: Clumsy Crucible
//  prep: 67.2ms
//  part 1: 778.ms - 694
//  part 2: 1.81s - 829
