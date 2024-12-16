package year2023.day16

import year2023.DayOf2023
import common.Default
import parse.{*, given}
import coord.{Dir, Pos, given}
import graph.traverse.BFS
import grid.{CharGrid, VectorGrid}

given Default[Char] = '.'

given Read[CharGrid] = VectorGrid.apply(_)

object Day16 extends DayOf2023[CharGrid](16, "The Floor Will Be Lava"):

  case class State(p: Pos, d: Dir)

  def neighbors(g: CharGrid)(st: State): Set[State] =
    val newD = g(st.p) -> st.d match
      case ('\\', Dir.E | Dir.W) | ('/', Dir.N | Dir.S) => Set(st.d.right)
      case ('\\', Dir.S | Dir.N) | ('/', Dir.E | Dir.W) => Set(st.d.left)
      case ('|' , Dir.E | Dir.W) | ('-', Dir.N | Dir.S) => Set(st.d.right, st.d.left)
      case _                                            => Set(st.d)
    newD.map(d => State(st.p.toDir(d), d)).filter(st => g.gridBox.contains(st.p))

  def energized(g: CharGrid)(start: State): Int =
    //BFSExt(start, neighbors(g)).map(_.p).size
    // реализация этого traverse на порядок быстрее
    BFS.traverse(start)(neighbors(g)).keySet.map(_.p).size

  override def part1(cavern: CharGrid): Int =
    energized(cavern)(State(Pos.zero, Dir.E))

  override def part2(cavern: CharGrid): Int =
    val gb = cavern.gridBox
    val cs = (gb.dl.x to gb.ur.x).flatMap(x =>
      Set(State(Pos(x, gb.dl.y), Dir.N), State(Pos(x, gb.ur.y), Dir.S)))
    val rs = (gb.dl.y to gb.ur.y).flatMap(y =>
      Set(State(Pos(gb.dl.x, y), Dir.E), State(Pos(gb.ur.x, y), Dir.W)))
    (rs ++ cs).map(energized(cavern)).max

//Day 16: The Floor Will Be Lava
//  parse : 15.3ms
//  part 1: 170.ms -> 7884
//  part 2: 2.51s -> 8185
