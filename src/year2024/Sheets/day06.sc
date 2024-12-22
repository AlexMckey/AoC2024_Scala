import coord.{Dir, Pos}
import grid.{CharGrid, VectorGrid}

import scala.annotation.tailrec

case class State(p: Pos, d: Dir):
  def step: State = this.copy(p = p.toDir(d))
//  def goWhile(cond: State => Boolean): Iterator[State] =
//    Iterator.unfold(this){ st =>
//      if cond(st) then None
//      else Some(st -> step)
//    }

case class Walker(start: State)(using grid: CharGrid):
  def isStopWalk(st: State): Boolean = !grid.contains(st.p)
  def isBlocked(st: State): Boolean = grid.get(st.p).contains('#')
  def changeStateRule(st: State): State =
    val fwd = st.step
    if isBlocked(fwd)
    then st.copy(d = st.d.right).step
    else fwd
  def path: Seq[State] =
    val steps = LazyList.iterate(start -> Seq.empty[State]){ (st, path) =>
      val newState = changeStateRule(st)
      val newPath = path :+ newState
      (newState, newPath)
    }
    steps.takeWhile((st,_) => !isStopWalk(st)).last._2

val s = "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#..."
given g: CharGrid = VectorGrid(s)
val start = State(g.posOf('^').get, Dir.N)
val walker = Walker(start)
val path = walker.path
path.map(_.p).distinct.size

def countRightTurnLoops(path: Seq[State]): Int =
  @tailrec
  def rec(cur: State, passedPath: Seq[State], remainingPath: Seq[State], cnt: Int = 0)(using g: CharGrid): Int =
    if remainingPath.isEmpty then cnt
    else if !g.contains(cur.p) || g.get(cur.p).contains('#') then
      val st = remainingPath.head
      rec(st.copy(d = st.d.right), st +: passedPath, remainingPath.tail, cnt)
    else if passedPath.contains(cur) then
      val st = remainingPath.head
      rec(st.copy(d = st.d.right), st +: passedPath, remainingPath.tail, cnt + 1)
    else rec(cur.step, passedPath, remainingPath, cnt)
  val st = path.head  
  rec(st.copy(d = st.d.right), Seq.empty, path)
  
countRightTurnLoops(path)