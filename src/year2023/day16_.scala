package year2023.day16

import year2023.DayOf2023
import common.Default
import parse.{*, given}
import coord.{Dir, Pos, given}
import graph.BFS
import grid.{CharGrid, VectorGrid}
import line.Line
import memo.{*, given}

import scala.annotation.tailrec

object Day16_ extends DayOf2023[CharGrid](16, "The Floor Will Be Lava"):

  given graph: Cache[State, StateLen] = Cache.empty

  case class State(p: Pos, d: Dir):
    def opposite: State = copy(d = d.flip)

  case class StateLen(st: State, len: Int)

  def neighbors(st: State)(using g: CharGrid): Set[State] =
    val newD = g.getOrElse(st.p, '.') -> st.d match
      case ('\\', Dir.E | Dir.W) | ('/', Dir.N | Dir.S) => Set(st.d.right)
      case ('\\', Dir.S | Dir.N) | ('/', Dir.E | Dir.W) => Set(st.d.left)
      case ('|', Dir.E | Dir.W) | ('-', Dir.N | Dir.S)  => Set(st.d.right, st.d.left)
      case _                                            => Set(st.d)
    newD.map(d => st.copy(d = d))

  def findNextObj(st: State)(using g: CharGrid, graph: Cache[State, StateLen]): Option[State] =
    @tailrec
    def rec(st: State, len: Int = 0): StateLen =
      val newSt = st.copy(p = st.p.toDir(st.d))
      g.get(newSt.p) match
        case None      => StateLen(st, len)
        case Some('.') => rec(newSt, len + 1)
        case _         => StateLen(newSt, len + 1)

    val res = rec(st)
    if res.len != 0
    then
      graph += st              -> res
      graph += res.st.opposite -> StateLen(st.opposite, res.len)
      Some(res.st)
    else None

  def step(st: State)(using g: CharGrid, graph: Cache[State, StateLen]): Set[State] =
    findNextObj(st).map(neighbors).getOrElse(Set.empty)

  def energized(start: State)(using g: CharGrid): Int =
    BFS.traverse(start, step)
       .keys
       .map(st1 =>
          graph
            .get(st1)
            .map(st2 => Line(st1.p, st2.st.p)))
       .filter(_.isDefined)
       .flatMap(_.get.expand)
       .size

  override def part1(cavern: CharGrid): Int =
    given CharGrid = cavern
    energized(State(Pos(-1, 0), Dir.E)) - 1

  override def part2(cavern: CharGrid): Int =
    given CharGrid = cavern
    val cs = (cavern.minPos.x to cavern.maxPos.x).flatMap(x =>
      Set(State(Pos(x, cavern.minPos.y), Dir.N), State(Pos(x, cavern.maxPos.y), Dir.S))
    )
    val rs = (cavern.minPos.y to cavern.maxPos.y).flatMap(y =>
      Set(State(Pos(cavern.minPos.x, y), Dir.E), State(Pos(cavern.maxPos.x, y), Dir.W))
    )
    Set(rs, cs).flatten.map(energized).max

//Day 16: The Floor Will Be Lava
//  prep: 48.9ms
//  part 1: 76.1ms - 7884
//  part 2: 730.ms - 8185
