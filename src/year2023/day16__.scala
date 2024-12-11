package year2023.day16

import year2023.DayOf2023
import common.Default
import parse.{*, given}
import coord.{Dir, Pos, given}
import grid.{CharGrid, VectorGrid}
import line.{Line, LineKind}
import memo.{*, given}
import traverse.BFS

import scala.annotation.tailrec

object Day16__ extends DayOf2023[CharGrid](16, "The Floor Will Be Lava"):

  given graph: Cache[State, Set[State]] = Cache.empty

  case class State(p: Pos, d: Dir):
    def opposite: State = copy(d = d.flip)

  def neighbors(st: State)(using g: CharGrid): Set[State] =
    val newD = g.getOrElse(st.p, '.') -> st.d match
      case ('\\', Dir.E | Dir.W) | ('/', Dir.N | Dir.S) => Set(st.d.right)
      case ('\\', Dir.S | Dir.N) | ('/', Dir.E | Dir.W) => Set(st.d.left)
      case ('|', Dir.E | Dir.W) | ('-', Dir.N | Dir.S)  => Set(st.d.right, st.d.left)
      case _                                            => Set(st.d)
    newD.map(d => st.copy(d = d))

  def findNextObj(st: State)(using g: CharGrid): Option[State] =
    @tailrec
    def lineToNextObj(curP: Pos): Pos =
      val newP = curP.toDir(st.d)
      if g.get(newP).isEmpty
      then curP
      else if g(newP) != '.'
      then newP
      else lineToNextObj(newP)
    val newP = lineToNextObj(st.p)
    val line = Line(st.p, newP)
    Option.when(line.kind != LineKind.Dot)(State(newP, st.d))

  def step(st: State)(using g: CharGrid, graph: Cache[State, Set[State]]): Set[State] =
    Memoize(st) { neighbors(st).flatMap(findNextObj) }

  def energized(start: State)(using g: CharGrid): Int =
    BFS
      .traverse(start, step)
      .keySet
      .map(st => st -> graph.get(st).get)
      .flatMap((st, set) => set.map(ss => Line(st.p, ss.p).expand))
      .flatten
      .size

  override def part1(cavern: CharGrid): Int =
    given CharGrid = cavern
    energized(State(Pos(-1, 0), Dir.E)) - 1

  override def part2(cavern: CharGrid): Int =
    given CharGrid = cavern
    val cs = (cavern.minPos.x to cavern.maxPos.x)
      .flatMap(x => Set(State(Pos(x, cavern.minPos.y), Dir.N), State(Pos(x, cavern.maxPos.y), Dir.S)))
    val rs = (cavern.minPos.y to cavern.maxPos.y)
      .flatMap(y => Set(State(Pos(cavern.minPos.x, y), Dir.E), State(Pos(cavern.maxPos.x, y), Dir.W)))
    Set(rs, cs).flatten.map(energized).max

//Day 16: The Floor Will Be Lava
//  prep: 106.ms
//  part 1: 189.ms - 7884
//  part 2: 4.44s - 8185
