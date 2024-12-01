package year2023.day16

import puzzle.Puzzle
import common.Default
import parse.{*, given}
import coord.{Dir, Pos, given}
import graph.BFSExt
import grid.{CharGrid, VectorGrid}

given Default[Char] = '.'

given Read[CharGrid] = VectorGrid.apply(_)

object Day16 extends Puzzle[CharGrid](2023, 16, "The Floor Will Be Lava"):

  case class State(p: Pos, d: Dir)

  def neighbors(g: CharGrid)(st: State): Set[State] =
    val newD = g(st.p) -> st.d match
      case ('\\', Dir.E | Dir.W) | ('/', Dir.N | Dir.S) => Set(st.d.right)
      case ('\\', Dir.S | Dir.N) | ('/', Dir.E | Dir.W) => Set(st.d.left)
      case ('|' , Dir.E | Dir.W) | ('-', Dir.N | Dir.S) => Set(st.d.right, st.d.left)
      case _                                            => Set(st.d)
    newD.map(d => State(st.p.toDir(d), d)).filter(st => g.gridBox.contains(st.p))

  def energized(g: CharGrid)(start: State): Int = BFSExt(start, neighbors(g)).map(_.p).size

//  override def prep(input: String): CharGrid = VectorGrid(input)

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
//  prep: 64.3ms
//  part 1: 1.50s - 7884
//  part 2: 339.s - 8185
