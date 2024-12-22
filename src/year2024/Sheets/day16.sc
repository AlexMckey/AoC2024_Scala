import grid.{CharGrid, VectorGrid}
import coord.{Pos, Dir}
import graph.traverse.old.Dijkstra

//val s = "###############\n#.......#....E#\n#.#.###.#.###.#\n#.....#.#...#.#\n#.###.#####.#.#\n#.#.#.......#.#\n#.#.#####.###.#\n#...........#.#\n###.#.#####.#.#\n#...#.....#.#.#\n#.#.#.###.#.#.#\n#.....#...#.#.#\n#.###.#.#.#.#.#\n#S..#.....#...#\n###############"
val s = "#################\n#...#...#...#..E#\n#.#.#.#.#.#.#.#.#\n#.#.#.#...#...#.#\n#.#.#.#.###.#.#.#\n#...#.#.#.....#.#\n#.#.#.#.#.#####.#\n#.#...#.#.#.....#\n#.#.#####.#.###.#\n#.#.#.......#...#\n#.#.###.#####.###\n#.#.#...#.....#.#\n#.#.#.#####.###.#\n#.#.#.........#.#\n#.#.#.#########.#\n#S#.............#\n#################"

val m = VectorGrid(s)

case class State(p: Pos, d: Dir)

val start = State(m.posOf('S').get, Dir.E)
val end = m.posOf('E').get

def ns(st: State): Seq[(State, Int)] = {
  val newP = st.p.toDir(st.d)
  if m(newP) != '#'
  then (State(newP, st.d) -> 1) +: Seq(st.copy(d = st.d.left) -> 1000, st.copy(d = st.d.right) -> 1000)
  else Seq(st.copy(d = st.d.left) -> 1000, st.copy(d = st.d.right) -> 1000)
}

def backns(st: State): Seq[(State, Int)] = {
  val newP = st.p.toDir(st.d.flip)
  if m(newP) != '#'
  then (State(newP, st.d) -> 1) +: Seq(st.copy(d = st.d.left) -> 1000, st.copy(d = st.d.right) -> 1000)
  else Seq(st.copy(d = st.d.left) -> 1000, st.copy(d = st.d.right) -> 1000)
}

def findBestPos(st: State)(using res: Map[State, Int]): Set[Pos] = {
  if st == start
  then Set(st.p)
  else {
    val ds = res(st)
    val ps =
      for {
        (oldSt, step) <- backns(st)
        oldD <- res.get(oldSt)
        if oldD + step == ds
      } yield findBestPos(oldSt) + st.p
    ps.foldLeft(Set.empty)(_ ++ _)
  }
}
Dijkstra.search[State](start, _.p == end, ns)

given res: Map[State,Int] = Dijkstra.traverse[State](start, ns)
val goal = res.filter(_._1.p == end).minBy(_._2)._1

findBestPos(goal).size
