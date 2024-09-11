import AoCLib.exts.*
import AoCLib.exts.IteratorExts.*
import AoCLib.grid.MapGrid
import AoCLib.graph.{BFSExt,BFS}
import AoCLib.pos.{Neighbors, Pos, posOrdering}
import AoCLib.dir.Dir
import AoCLib.line.{Line, LineKind}
import AoCLib.exts.Memoized.*

import scala.annotation.tailrec
import scala.util.chaining.*

val s =
  ".|...\\....\n|.-.\\.....\n.....|-...\n........|.\n..........\n.........\\\n..../.\\\\..\n.-.-/..|..\n.|....-|.\\\n..//.|...."

case class State(p: Pos, d: Dir):
  def opposite: State = copy(d = d.opposite)

given g: MapGrid = MapGrid(s)

given graph: Cache[State, Set[State]] = Cache.empty

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
  //println(line)
//  if line.kind != LineKind.Dot
//  then
//    graph += st -> line
//    graph += State(newP, st.d.opposite) -> line
//    Some(State(newP, st.d))
//  else None

def step(st: State)(using g: MapGrid, graph: Cache[State, Set[State]]): Set[State] =
  Memoize(st) { neighbors(st).flatMap(findNextObj) }
  //neighbors(st).flatMap{sti =>
    //val newStOp =
  //  Memoize(sti){findNextObj(sti)}
//    println(sti)
//    println(newStOp)
//    if newStOp.isDefined
//    then graph += newStOp.get.opposite -> Some(st.opposite)
//    newStOp
  //}//.filter(_.isDefined).map(_.get)

g.get(Pos(1,0))

val l = Line(Pos.zero,Pos.zero)
l.kind

var st0 = neighbors(start)
val st0_1 = st0.map(findNextObj)
graph

neighbors(State(Pos(1,0),Dir.E))

var ss = Set(start)
ss = ss.flatMap(step)
ss = ss.flatMap(step)
ss = ss.flatMap(step)
ss = ss.flatMap(step)
ss = ss.flatMap(step)
ss = ss.flatMap(step)
ss = ss.flatMap(step)
ss = ss.flatMap(step)
ss = ss.flatMap(step)
ss = ss.flatMap(step)
ss = ss.flatMap(step)
ss = ss.flatMap(step)
ss = ss.flatMap(step)
ss = ss.flatMap(step)
graph

//val res_ = Iterator.iterate(Set(start)){st => st.flatMap(step)}.flatten.take(120)//.zipWithPrev.takeWhile((i1,i2) => i1 == i2)
//res_.toSeq//.flatten.distinct.flatMap(graph.get)

val res1 = BFSExt[State](start, step)
val res2 = BFS.traverse(start, step).keySet
graph

val resc = res1.toSeq.sortBy(_.p)
resc.map(st => st -> graph.get(st).get)
    .flatMap((st, set) => set.map(ss => Line(st.p, ss.p).expand))
    .flatten.distinct.size