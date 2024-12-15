import grid.{CharGrid, Direction, VectorGrid}
import coord.{Dir, GridDir, Pos}
import GridDir.*
import coord.Dir.*
import grid.Direction.*
import walker.{Travel, Walker}

import scala.annotation.tailrec
import scala.collection.mutable

//val s = "########\n#..O.O.#\n##@.O..#\n#...O..#\n#.#.O..#\n#...O..#\n#......#\n########\n\n<^^>>>vv<v>>v<<"
val s = "##########\n#..O..O.O#\n#......O.#\n#.OO..O.O#\n#..O@..O.#\n#O#..O...#\n#O..O..O.#\n#.OO.O.OO#\n#....O...#\n##########\n\n<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\nvvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\nv^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
val Array(mazes, movess) = s.split("\n\n")
val ms = movess.replaceAll("\n","").toCharArray
val g = VectorGrid(mazes)

extension (mz: CharGrid)
  def GPScore: Long =
    mz.findAll('O').foldLeft(0L): (acc, p) =>
      acc + p.y * 100 + p.x

VectorGrid("##########\n#.O.O.OOO#\n#........#\n#OO......#\n#OO@.....#\n#O#.....O#\n#O.....OO#\n#O.....OO#\n#OO....OO#\n##########")
  .GPScore
VectorGrid("########\n#....OO#\n##.....#\n#.....O#\n#.#O@..#\n#...O..#\n#...O..#\n########")
  .GPScore

val wall = '#'
val stone = 'O'
val empty = '.'
val robot = '@'
val start: Pos = g.find('@').get

def ray(g: CharGrid, p: Pos)(d: Dir): Seq[Char] =
  @tailrec
  def rec(curP: Pos, acc: Seq[Char] = Seq.empty): Seq[Char] =
    val nextP = curP.toDir(d)
    val ch = g(nextP)
    ch match
      case `wall` => Seq(ch)
      case `stone` => rec(nextP, ch +: acc)
      case `empty` => ch +: acc
  rec(p)

ray(g, start)('<'.asGridDir)
Iterator.iterate(start -> Seq(g(start))){ (p, seq) =>
  val newP = p.toDir(R)
  newP -> (g(newP) +: seq)
}.dropWhile(_._2.head == 'O').next()._2

case class State(g: CharGrid, p: Pos):
  def step(ch: Char): State = step(ch.asGridDir)
  def step(d: Dir): State =
    val sps = ray(g, p)(d)
    val newP = p.toDir(d)
    sps match
      case Nil | Seq(`wall`) => State(g, p)
      case `wall` +: _ :+ `stone` => State(g, p)
      case `empty` +: ss :+ `stone` =>
        State(g.updated(newP, robot)
          .updated(newP.toDir(d,ss.length+1), stone)
          .updated(p, empty), newP)
      case Seq(`empty`, _*) =>
        State(g.updated(newP, robot).updated(p, empty), newP)

val st1 = State(g, start)
st1.p
st1.p.toDir('>'.asGridDir,2)
st1.p.toDir('v'.asGridDir,2)
st1.step('<')
st1.step('^')
st1.step('v')
val st2 = st1.step('>')

st2.step('<')
st2.step('^')
st2.step('v')
ray(st2.g, st2.p)('>'.asGridDir)
val st3 = st2.step('>')

st3.step('<')
st3.step('^')
st3.step('v')
st3.step('>')


val res = ms.foldLeft(State(g,start)): (st, d) =>
  st.step(d)

res.g.GPScore
