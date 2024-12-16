import coord.{Dir, Pos}
import walker.Walker
import grid.{CharGrid, VectorGrid}
import graph.traverse.DFS

val s = "###############\n#.......#....E#\n#.#.###.#.###.#\n#.....#.#...#.#\n#.###.#####.#.#\n#.#.#.......#.#\n#.#.#####.###.#\n#...........#.#\n###.#.#####.#.#\n#...#.....#.#.#\n#.#.#.###.#.#.#\n#.....#...#.#.#\n#.###.#.#.#.#.#\n#S..#.....#...#\n###############"
val g = VectorGrid(s)

val start = g.find('S').get
val end = g.find('E').get

def ns(st: Walker): Seq[Walker] =
  val next = st.step
  (if g(next.p) != '#' then Seq(next) else Seq.empty) ++
    Seq(st.turnLeft, st.turnRight)

def next(st: Walker): Seq[Walker] =
  val next = st.step
  if g(next.p) != '#' then Seq(next, st.turnLeft) else Seq(st.turnLeft)

val res1 = DFS(Walker(start, Dir.E))(ns)
res1.size
val res2 = DFS.traverse(Walker(start, Dir.E))(ns)
res2._2.size
res2._1.filter(_._1.p == end).minBy(_._2)
val res3 = DFS.search(Walker(start, Dir.E), _.p == end)(next)
res3.map(_.size).sorted
res3.size