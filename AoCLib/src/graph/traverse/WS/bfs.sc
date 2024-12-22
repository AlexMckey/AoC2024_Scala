import common.time
import coord.{Dir, Pos, Coord, given}
import walker.Walker
import grid.{CharGrid, VectorGrid}
import graph.traverse.BFS

val s = "###############\n#.......#....E#\n#.#.###.#.###.#\n#.....#.#...#.#\n#.###.#####.#.#\n#.#.#.......#.#\n#.#.#####.###.#\n#...........#.#\n###.#.#####.#.#\n#...#.....#.#.#\n#.#.#.###.#.#.#\n#.....#...#.#.#\n#.###.#.#.#.#.#\n#S..#.....#...#\n###############"
val g = VectorGrid(s)

val start = g.posOf('S').get
val end = g.posOf('E').get

def ns(st: Walker): Seq[Walker] =
  val next = st.step
  (if g(next.p) != '#' then Seq(next) else Seq.empty) ++
    Seq(st.turnLeft, st.turnRight)

def next(st: Walker): Seq[Walker] =
  val next = st.step
  if g(next.p) != '#' then Seq(next, st.turnLeft) else Seq(st.turnLeft)

def cns(p: Pos): Iterable[Pos] =
  p.nearAxis.filter(g.contains).filter(g(_) == g(p)).toSeq

val res1 = BFS.components(g.allPos)(cns)
res1.size
res1.toList.map(s => g(s.head))

val (res2, t2) = time(BFS.traverse(Walker(start, Dir.E))(ns))
t2
res2.size
res2.filter(_._1.p == end).minBy(_._2)
res2.toList.sortBy(_._2)

val res3 = BFS.stepCount(Walker(start, Dir.E), _.p == end)(ns)

val (res4, res5) = BFS.search(Walker(start, Dir.E), _.p == end)(ns)
res4.isEmpty
res4.get

res5.size
res5.toList.filter(_._1.p == end)