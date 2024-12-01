import AoCLib.exts.*
import AoCLib.{Puzzle, Grid as _}
import AoCLib.grid.MapGrid
import AoCLib.pos.{Pos, axisNeighbors}
import AoCLib.dir.*
import AoCLib.graph.*

import scala.annotation.tailrec

case class HikePos(pos: Pos, path: List[Pos], pathSet: Set[Pos])

val s = "#.#####################\n#.......#########...###\n#######.#########.#.###\n###.....#.>.>.###.#.###\n###v#####.#v#.###.#.###\n###.>...#.#.#.....#...#\n###v###.#.#.#########.#\n###...#.#.#.......#...#\n#####.#.#.#######.#.###\n#.....#.#.#.......#...#\n#.#####.#.#.#########v#\n#.#...#...#...###...>.#\n#.#.#v#######v###.###v#\n#...#.>.#...>.>.#.###.#\n#####v#.#.###v#.#.###.#\n#.....#...#...#.#.#...#\n#.#########.###.#.#.###\n#...###...#...#...#.###\n###.###.#.###v#####v###\n#...#...#.#.>.>.#.>.###\n#.###.###.#.###.#.#v###\n#.....###...###...#...#\n#####################.#"

def slopestoDir(slope: Char): Set[Dir] =
  slope match
    case '^' => Set(Dir.N)
    case 'v' => Set(Dir.S)
    case '>' => Set(Dir.E)
    case '<' => Set(Dir.W)
    case '.' => Dir.values.toSet

val g = MapGrid(s)
val start = Pos(g.row(0).indexOf("."), 0)
val end = Pos(g.row(g.maxRow).indexOf("."), g.maxRow)

val branchPoss = g.filter((p, c) => c != '#' &&
    g.neighborCount(p, _ != '#') > 2
  ).allPos + start + end

val keyPoss = branchPoss + start + end

val slopes = true

val keyNeighbors: Map[Pos, Map[Pos, Int]] =
  keyPoss.view.map { fromPos =>

    def neighbors(p: Pos): Iterator[Pos] =
      if p != fromPos && keyPoss(p)
      then Iterator.empty
      else
        val ns =
          if !slopes || g(p) == '.'
          then p.neighbors
          else slopestoDir(g(p)).map(p.toDir)
        ns.iterator.filter(newP => g.gridBox.contains(newP) && g(newP) != '#')

    val ds = BFS.traverse(fromPos, neighbors)
    fromPos -> ds.filter((p,_) => p != fromPos && keyPoss(p))
  }.toMap

val startNode: HikePos = HikePos(start, List(), Set(start))

def neighborsNode(hikePos: HikePos): IterableOnce[(HikePos, Int)] =
    if hikePos.pos != end
    then
      val HikePos(pos, path, pathSet) = hikePos
      for
        (newPos, dist) <- keyNeighbors(pos)
        if !pathSet.contains(newPos)
      yield HikePos(newPos, path, pathSet + newPos) -> dist
    else
      Iterator.empty

@tailrec
def helper(todo: Set[HikePos], visited: Map[HikePos, Int]): Map[HikePos, Int] = {
  if (todo.isEmpty)
    visited.filter(_._1.pathSet.contains(end))
  else {
    val hikePos = todo.head
    val newTodo = todo - hikePos
    val visited0 = visited
    val oldDist = visited0(hikePos)
    val (newTodo2, newVisited) = neighborsNode(hikePos).iterator.foldLeft((newTodo, visited))({ case ((todo, visited), (newHikePos2, dist)) =>
      val newDist = oldDist + dist
      if (visited0.contains(newHikePos2)) {
        if (visited0(newHikePos2) < newDist) {
          (todo + newHikePos2, visited + (newHikePos2 -> newDist))
        }
        else {
          (todo, visited)
        }
      }
      else {
        (todo + newHikePos2, visited + (newHikePos2 -> newDist))
      }
    })
    helper(newTodo2, newVisited)
  }
}

val asd = helper(Set(startNode), Map(startNode -> 0))

asd.foreach(println)
asd
  .values
  .max

g.findAll(1, "[><^v]".r)
 .foldLeft(g)(_.updated(_, '.'))