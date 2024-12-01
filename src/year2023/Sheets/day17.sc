import AoCLib.dir.Dir
import AoCLib.grid.MapGrid
import AoCLib.graph.{AStarExt, BFS, Dijkstra}
import AoCLib.pos.{Pos, axisNeighbors, posOrdering}

import scala.math.abs
import scala.util.chaining.*

val s = "2413432311323\n3215453535623\n3255245654254\n3446585845452\n4546657867536\n1438598798454\n4457876987766\n3637877979653\n4654967986887\n4564679986453\n1224686865563\n2546548887735\n4322674655533"
given g: MapGrid = MapGrid(s)

extension (c: Char)
  def heatLoss: Int = c.asDigit

case class State(p: Pos, curDir: Dir = Dir.E, prevDirCnt: Int = 0)

def neighbors(canMove: (State, Dir) => Boolean)(st: State)(using g: MapGrid): Set[(State, Int)] =
  (if st.prevDirCnt == 0
   then Dir.allDirs.keySet
   else Dir.allDirs.keySet - st.curDir.opposite)
    .map(d => st.p.toDir(d) -> d)
    .filter((p, d) => g.gridBox.contains(p) && canMove(st,d))
    .map((p, d) => State(p, d, if d == st.curDir then st.prevDirCnt + 1 else 1) -> g(p).heatLoss)

def canMove_p1(st: State, d: Dir): Boolean = st.prevDirCnt < 3 || d != st.curDir

val goal = g.gridBox.max
val start = State(g.gridBox.min)
Dijkstra.search[State](start, _.p == goal, neighbors(canMove_p1))
        .get._2

val np1 = neighbors(canMove_p1)
var sts = np1(start)
sts = sts.flatMap((st,_) => np1(st))
sts = sts.flatMap((st,_) => np1(st))  