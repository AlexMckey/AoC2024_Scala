import AoCLib.exts.*
import AoCLib.grid.MapGrid
import AoCLib.graph.BFSExt
import AoCLib.pos.{Neighbors, Pos, posOrdering}
import AoCLib.dir.Dir
import AoCLib.line.Line
import AoCLib.exts.Memoized.*

import scala.annotation.tailrec
import scala.util.chaining.*

val s =
  ".|...\\....\n|.-.\\.....\n.....|-...\n........|.\n..........\n.........\\\n..../.\\\\..\n.-.-/..|..\n.|....-|.\\\n..//.|...."

case class State(p: Pos, d: Dir):
  def opposite: State = copy(d = d.opposite)

case class StateLen(st: State, len: Int)

given g: MapGrid = MapGrid(s)
given graph: Cache[State, StateLen] = Cache.empty

val start: State = State(Pos.zero, Dir.E)

def neighbors(st: State)(using g: MapGrid): Set[State] =
  val newD = g.getOrElse(st.p, '.') -> st.d match
    case ('\\', Dir.E | Dir.W) | ('/', Dir.N | Dir.S) => Set(st.d.turnRight)
    case ('\\', Dir.S | Dir.N) | ('/', Dir.E | Dir.W) => Set(st.d.turnLeft)
    case ('|', Dir.E | Dir.W) | ('-', Dir.N | Dir.S)  => Set(st.d.turnRight, st.d.turnLeft)
    case _                                            => Set(st.d)
  newD.map(d => st.copy(d = d))

def findNextObj(st: State)(using g: MapGrid): Option[State] =
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
    graph += st -> res
    graph += res.st.opposite -> StateLen(st.opposite, res.len)
    Some(res.st)
  else None

g.get(Pos(1,0))

var st0 = findNextObj(start)
graph
var st0s = neighbors(st0.get)
var st0_1 = findNextObj(st0s.head)
var st0_2 = findNextObj(st0s.last)
graph
st0s = neighbors(st0_1.get)
st0_1 = findNextObj(st0s.head)
st0_2 = findNextObj(st0s.last)
graph

def step(st: State): Set[State] =
  val end = findNextObj(st)
  if end.isDefined
  then neighbors(end.get)
  else Set.empty

val s1 = step(start)
val s2 = step(s1.head)
val s3 = step(s1.last)
val s4 = step(s2.head)
val s5 = step(s2.last)

val res = BFSExt[State](start, step)
val ress = res.toSeq.sortBy(_.p)
val resc = res
  .map(st1 => graph
    .get(st1)
    .map(st2 => Line(st1.p, st2.st.p)))
  .filter(_.isDefined)
  .flatMap(_.get.expand)

resc.size