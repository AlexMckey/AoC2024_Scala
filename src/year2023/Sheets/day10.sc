import scala.annotation.tailrec
//import AoCLib.exts.GridExts.*
import scala.util.chaining.*
import AoCLib.coord.{Pos, given}

//val s = "-L|F7\n7S-7|\nL|7||\n-L-J|\nL|-JF"
//val s = "..F7.\nJFJ|.\nSJ.L7\n|F--J\nLJ..."
//val s = "...........\n.S-------7.\n.|F-----7|.\n.||.....||.\n.||.....||.\n.|L-7.F-J|.\n.|..|.|..|.\n.L--J.L--J.\n..........."
//val s = "..........\n.S------7.\n.|F----7|.\n.||....||.\n.||....||.\n.|L-7F-J|.\n.|..||..|.\n.L--JL--J.\n.........."
//val s = ".F----7F7F7F7F-7....\n.|F--7||||||||FJ....\n.||.FJ||||||||L7....\nFJL7L7LJLJ||LJ.L-7..\nL--J.L7...LJS7F-7L7.\n....F-J..F7FJ|L7L7L7\n....L7.F7||L7|.L7L7|\n.....|FJLJ|FJ|F7|.LJ\n....FJL-7.||.||||...\n....L---J.LJ.LJLJ..."
val s = "FF7FSF7F7F7F7F7F---7\nL|LJ||||||||||||F--J\nFL-7LJLJ||||||LJL-77\nF--JF--7||LJLJ7F7FJ-\nL---JF-JLJ.||-FJLJJ7\n|F|F-JF---7F7-L7L|7|\n|FFJF7L7F-JF7|JL---7\n7-L-JL7||F7|L7F-7F7|\nL.L7LFJ|||||FJL7||LJ\nL7JLJL-JLJLJL--JLJ.L"

type Board = Grid[Char]

var g: Grid[Char] = VectorGrid(s)

val pipes: Map[Char, Set[Dir]] = Map(
  '.' -> Set.empty,
  '-' -> Set(W, E),
  '|' -> Set(S, N),
  'F' -> Set(S, E),
  'L' -> Set(E, N),
  'J' -> Set(N, W),
  '7' -> Set(W, S)
)

def nearPipes(g: Grid[Char])(p: Pos): Set[Pos] =
  pipes(g.getOrElse(p, '.')).map(p.toDir) - p

val start      = g.find('S').getOrElse(Pos.zero)
val startNears = start.near4.filter(n => nearPipes(g)(n).contains(start)).map(_ - start).toSet
val startChar  = pipes.map(_.swap)(startNears.map(asDir))
g = g.updated(start, startChar)

def costs(c: Char): Int = c match
  case '-'       => 0
  case '|'       => 1
  case 'F' | '7' => 2
  case 'L' | 'J' => -1
  case _         => 0

import AoCLib.graph.DFS

val res = DFS.traverse(start, nearPipes(g))
val path = res._2

val mm = Map(Set(Pos(-1,0),Pos(0,1)) -> 'S')
mm(Set(Pos(-1,0),Pos(0,1)))
mm(Set(Pos(0,1),Pos(-1,0)))

val y = 3
val pathRow = path.filter(_.y == y).sortBy(_.x)
val pathMinX = pathRow.min.x
val pathMaxX = pathRow.max.x
(pathMinX to pathMaxX).foldLeft(0 -> 0){ case ((cnt, cur), i) =>
  val p = Pos(i,y)
  //println(p)
  if pathRow.contains(p)
  then cnt -> (cur + costs(g(p)))
  else if cur % 2 != 0
  then (cnt + 1) -> cur
  else cnt -> cur
}._1

@tailrec
def reps(str: Seq[Char], acc: String = ""): String = str match
  case s if s.isEmpty => acc
  case l :: '-' :: tl => reps(l :: tl, acc)
  case '|' :: tl => reps(tl, acc + "|")
  case 'F' :: 'J' :: tl => reps(tl, acc + '|')
  case 'L' :: '7' :: tl => reps(tl, acc + '|')
  case _ => reps(str.tail, acc)

def tilesAndPathInRow(y: Int, path: Seq[Pos], g: Grid[Char]): Int =
  val pathRow = path.filter(_.y == y).sortBy(_.x)
  val gc = pathRow.map(p => p.x -> g.apply(p))
  pathRow
    .map(_.x)
    .sliding(2)
    .map{ case List(l: Int, r: Int) => l -> (r-l-1) }
    .filterNot(_._2 == 0)
    .toList
    .map((px, l) => l -> gc.takeWhile(_._1 <= px).map(_._2))
    .map((l,s) => l -> (reps(s).length % 2 != 0))
    .filter(_._2).map(_._1).sum

val gc = pathRow.map(p => p.x -> g.apply(p))
val ar = pathRow
  .map(_.x)
  .sliding(2)
  .map{ case List(l: Int, r: Int) => l -> (r-l-1) }
  .filterNot(_._2 == 0)
  .toList
val r2 = ar.map((px, l) => l -> gc.takeWhile(_._1 <= px).map(_._2))
r2.map((l,s) => l -> (reps(s).length % 2 != 0))
  .filter(_._2).map(_._1).sum

def enclosedTilesRow(y: Int, path: Seq[Pos], g: Grid[Char]): Int =
  //import scala.math.Ordering.Implicits.infixOrderingOps
  val pathRow  = path.filter(_.y == y).sortBy(_.x)

  pathRow.map(_.x)
         .sliding(2)
         .filter(_.reduce(_ - _) < -1)
         .collect { case List(l: Int, r: Int) if
           pathRow.filter(p => p.x <= l)
                  .foldLeft(0)((acc, p) => acc + costs(g(p))) % 2 != 0
         => r - l - 1
         }.sum

def countInRow(y: Int, path: Seq[Pos], g: Grid[Char]): Int =
  val pathRow = path.filter(_.y == y).sortBy(_.x)
  val pathMinX = pathRow.min.x
  val pathMaxX = pathRow.max.x
  (pathMinX to pathMaxX).foldLeft(0 -> 0){ case ((cnt, cur), i) =>
    val p = Pos(i,y)
    //println(p)
    if pathRow.contains(p)
    then cnt -> (cur + costs(g(p)))
    else if cur % 2 != 0
    then (cnt + 1) -> cur
    else cnt -> cur
  }._1

enclosedTilesRow(y, path, g)
tilesAndPathInRow(y, path, g)
countInRow(y, path, g)
g.row(y)
val ng = g.toMapGrid.keepOnlyInPositions(path.toSet)

ng.row(y).scanLeft(' ' -> 0){ case ((_, acc), c) => c -> (acc + costs(c))}.tail

val cps1 = (path.min.y until path.max.y)
  .map(tilesAndPathInRow(_, path, g)).sum

val cps2 = (path.min.y until path.max.y)
  .map(enclosedTilesRow(_, path, g)).sum

val cps3 = (path.min.y until path.max.y)
  .map(countInRow(_, path, g)).sum

import AoCLib.geometry.Geometry.*
val cps4 = polygonArea(path) - path.length / 2 + 1

-2 % 2 != 0