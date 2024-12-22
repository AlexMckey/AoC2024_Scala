import coord.{Coord, Dir, Pos, pos}
import graph.traverse.BFS
import grid.VectorGrid
import exts.iterables.groupCount

val s = "###############\n#...#...#.....#\n#.#.#.#.#.###.#\n#S#...#.#.#...#\n#######.#.#.###\n#######.#.#...#\n#######.#.###.#\n###..E#...#...#\n###.#######.###\n#...###...#...#\n#.#####.#.###.#\n#.#...#.#.#...#\n#.#.#.#.#.#.###\n#...#...#...###\n###############"

val g = VectorGrid(s)
val start = g.posOf('S').get
val end = g.posOf('E').get

def ns(p: Pos): Seq[Pos] =
  p.nearAxis.filter(g.contains).filterNot(g.isEq(_, '#')).toSeq

val (Some(target, len), path) = BFS.search(start, end)(ns)

val ps = path.flatMap((oldp,i) =>
  Dir.axisDirs.map(d => oldp.toDir(d,2)).filter(path.contains)
    .map(newp => path(newp) - i - 2)
)
ps.filter(_ > 0).groupCount(identity)

def cheatSaves(cheat: Int => Boolean): Iterable[Int] =
  for
    (p1, d1) <- path
    (p2, d2) <- path
    d12 = p1.manhattan(p2)
    if cheat(d12)// && d2 - d1 - d12 >= 20
  yield d2 - d1 - d12
//  path.flatMap { case (p1, d1) =>
//    path.filter { case (p2, d2) =>
//      val d12 = p1.manhattan(p2)
//      fn(d12) && d2 - d1 - d12 >= 20
//    }
//  }.size

cheatSaves(_ == 2).groupCount(identity).filter(_._1 > 0)