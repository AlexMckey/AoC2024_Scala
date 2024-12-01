import exts.*
import grid.{CharGrid, MapGrid}
import graph.BFSExt
import coord.{Pos, Neighbor, given}
import coord.Dir

val s =
  ".|...\\....\n|.-.\\.....\n.....|-...\n........|.\n..........\n.........\\\n..../.\\\\..\n.-.-/..|..\n.|....-|.\\\n..//.|...."

given g: CharGrid = MapGrid(s)

case class State(p: Pos, d: Dir)

val start = State(Pos.zero, Dir.E)

def neighbors(st: State)(using g: CharGrid): Set[State] =
  val newD = g(st.p) -> st.d match
    case ('\\', Dir.E | Dir.W) | ('/', Dir.N | Dir.S) => Set(st.d.right)
    case ('\\', Dir.S | Dir.N) | ('/', Dir.E | Dir.W) => Set(st.d.left)
    case ('|' , Dir.E | Dir.W) | ('-', Dir.N | Dir.S) => Set(st.d.right, st.d.right)
    case _                                            => Set(st.d)
  newD.map(d => State(st.p.toDir(d), d)).filter(st => g.gridBox.contains(st.p))

var st = neighbors(start)
neighbors(st.head)
val st1 = State(Pos(1, 7), Dir.S)
neighbors(st1)

val se = "######....\n.#...#....\n.#...#####\n.#...##...\n.#...##...\n.#...##...\n.#..####..\n########..\n.#######..\n.#...#.#.."
val ge = MapGrid(se)
ge.count(_ == '#')

val res = BFSExt(start, neighbors).map(_.p).size

import line.Line

val bx  = ge.gridBox
Seq(Line(bx.dl, bx.dl.copy(y = bx.ur.y)), 
    Line(bx.dl, bx.dl.copy(x = bx.ur.x)),
    Line(bx.dl.copy(x = bx.ur.x), bx.ur),
    Line(bx.dl.copy(y = bx.ur.y), bx.ur))
  .flatMap(_.expand).size

