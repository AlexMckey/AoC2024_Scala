//import scala.util.chaining.*
import AoCLib.*
import AoCLib.charDefault
import AoCLib.exts.*
import AoCLib.exts.Iterables.*
import AoCLib.exts.Maps.*
import AoCLib.grid.{Grid, VectorGrid, MapGrid}
import AoCLib.coord.{Dir, Pos, given}
import AoCLib.graph.DFS
import Dir.*

type Board = Grid[Char]

//val s = "-L|F7\n7S-7|\nL|7||\n-L-J|\nL|-JF"
//val s = "..F7.\nJFJ|.\nSJ.L7\n|F--J\nLJ..."
//val s = "...........\n.S-------7.\n.|F-----7|.\n.||.....||.\n.||.....||.\n.|L-7.F-J|.\n.|..|.|..|.\n.L--J.L--J.\n..........."
//val s = "..........\n.S------7.\n.|F----7|.\n.||....||.\n.||....||.\n.|L-7F-J|.\n.|..||..|.\n.L--JL--J.\n.........."
val s = ".F----7F7F7F7F-7....\n.|F--7||||||||FJ....\n.||.FJ||||||||L7....\nFJL7L7LJLJ||LJ.L-7..\nL--J.L7...LJS7F-7L7.\n....F-J..F7FJ|L7L7L7\n....L7.F7||L7|.L7L7|\n.....|FJLJ|FJ|F7|.LJ\n....FJL-7.||.||||...\n....L---J.LJ.LJLJ..."
//val s = "FF7FSF7F7F7F7F7F---7\nL|LJ||||||||||||F--J\nFL-7LJLJ||||||LJL-77\nF--JF--7||LJLJ7F7FJ-\nL---JF-JLJ.||-FJLJJ7\n|F|F-JF---7F7-L7L|7|\n|FFJF7L7F-JF7|JL---7\n7-L-JL7||F7|L7F-7F7|\nL.L7LFJ|||||FJL7||LJ\nL7JLJL-JLJLJL--JLJ.L"

//val g: Grid[Char] = s.asStrs.map(_.toVector).toVector

var oldG: Board = VectorGrid(s)

val pipes = Map(
  '.' -> Set.empty,
  '-' -> Set(E, W),
  '|' -> Set(N, S),
  'F' -> Set(S, E),
  'L' -> Set(N, E),
  'J' -> Set(N, W),
  '7' -> Set(S, W)
)

def nearPipes(g: Board)(p: Pos): Set[Pos] =
  pipes(g.getOrElse(p, '.')).map(p + _.delta) - p

val start      = oldG.find('S').getOrElse(Pos.zero)
val startNears = start.near.filter(n => nearPipes(oldG)(n).contains(start)).toSet
val startDirs  = startNears.map(_ - start)
val startChar  = pipes.map(_.swap).getOrElse(startDirs.map(_.asDir), '.')
val g       = oldG.updated(start, startChar)
val path       = DFS.traverse(start, nearPipes(g))._2

def costs(c: Char): Int = c match
  case '.' => 0
  case '-' => 0
  case '|' => 1
  case 'F' | '7' => 1
  case 'L' | 'J' => -1

val r = 3
val p = MapGrid[Char](path.map(pt => pt -> g(pt)).toMap)
g.row(r)
p.row(r)
g.row(r).zip(p.row(r)).map((gc,pc) => if gc == pc then ' ' else gc)
val pathRow = path.filter(_.y == r)
  .sortBy(_.x)//(Ordering.Int.reverse)
val pathMinX = pathRow.min.x
val pathMaxX = pathRow.max.x

val tls = g.row(r).slice(pathMinX, pathMaxX+1)//.groupCount(identity)
tls.map(costs).sliding(2)
  .map(_.sum)
  //.map(_.sum % 2 != 0)
  .toList
  //.map(_.map(costs).sum % 2 != 0).toList//.count(_ % 2 != 0)

pathRow.map(g.apply)
pathRow
  .sliding(2)
  .map(ap => ap.head -> (ap.map(p.apply.andThen(costs)).sum % 2 != 0))
  .toList
//  .collect{ case ap if ap.map(g.apply.andThen(costs)).sum % 2 != 0 => ap.reduce(_ - _).x }
//  .toList
  //.sum

def tilesAndPathInRow(y: Int, path: Seq[Pos], g: Map[Pos, Char]): Int =
  val pathRow = path.filter(_.y == y)
  val pathMinX = pathRow.min.x
  val pathMaxX = pathRow.max.x
  val tls = g.keySet
    .toSeq
    .filter(p => p.y == y && p.x > pathMinX && p.x < pathMaxX)
    .filterNot(path.contains)
  tls.count(p => pathRow
                 .filter(_.x < p.x)
                 .map(g)
                 .map(costs)
                 .sum % 2 != 0)

def isInside(inside: Boolean, c: Char): Boolean = c match
  case '|' | 'F' | '7' => !inside
  case _ => inside

p.rows
  .flatMap(_.scanLeft(false -> false){ case ((_, status), char) => (char == summon[Default[Char]].default) -> isInside(status, char) }
  .map(_ && _))
  .count(identity)
//  .count(_._1 == true))
//  .sum