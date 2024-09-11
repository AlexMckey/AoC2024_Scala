import AoCLib.exts.*
import AoCLib.{Puzzle, Grid as _}
import AoCLib.grid.MapGrid
import AoCLib.pos.{Neighbors, Pos, axisNeighbors}
import AoCLib.graph.*

import scala.annotation.tailrec

val s = "...........\n.....###.#.\n.###.##..#.\n..#.#...#..\n....#.#....\n.##..S####.\n.##..#...#.\n.......##..\n.##.#.####.\n.##..##.##.\n..........."

val g = MapGrid(s)
val start = g.find('S').get
val sum = g.filter((p,c) => c != '#' && (p manhattan start) <= 5)
sum.count(_ != ' ') / 2 - 1
131 / 2

def neighbors(p: Pos): Set[Pos] =
  p.near4.filter(p => g.gridBox.contains(p) && g(p) != '#')

def visitStep(cntStep: Int, start: Pos, neighbors: Pos => Set[Pos]): Set[Pos] =
  @tailrec
  def visited(cntStep: Int, toVisit: Set[Pos]): Set[Pos] =
    if cntStep == 0 then toVisit
    else visited(cntStep - 1, toVisit.flatMap(neighbors) -- toVisit)
  visited(cntStep, Set(start))

val res = visitStep(6, start, neighbors)
res.size

-13 % 10

def convertCoord(g: MapGrid)(p: Pos): Pos =
  val newX = (g.maxCol + p.x % (g.maxCol + 1) + 1) % (g.maxCol + 1)
  val newY = (g.maxRow + p.y % (g.maxRow + 1) + 1) % (g.maxCol + 1)
  Pos(newX, newY)

convertCoord(g)(Pos(-1,-1))
convertCoord(g)(Pos(13,13))
convertCoord(g)(Pos(1,1))

def neighborsEnlarge(g: MapGrid)(p: Pos): Set[Pos] =
  val (inPlace, outPlace) = p.near4.partition(g.gridBox.contains)
  inPlace.filter(g(_) != '#') ++ outPlace.filter(p => g(convertCoord(g)(p)) != '#')

val resb1 = visitStep(10, start, neighborsEnlarge(g))
resb1.size

def visitSteps(ss: Seq[Int], neighbors: Pos => Set[Pos]): Seq[Int] =
  @tailrec
  def visited(toVisit: Set[Pos], cntStep: Int): Set[Pos] =
    if cntStep == 0 then toVisit
    else visited(toVisit.flatMap(neighbors), cntStep - 1)

  ss.scanLeft(Set(start))(visited).tail.map(_.size)

visitSteps(Seq(11 / 2 + 1, 11 / 2 + 1 + 11, 11 / 2 + 1 + 11 * 2), neighborsEnlarge(g))

//val resb2 = visitSteps(50, start, neighborsEnlarge(g))
//resb2.size
//
//val resb3 = visitSteps(500, start, neighborsEnlarge(g))
//resb3.size

type PosInt = (Pos, Int)

val walls = g.findAll(1, "#".r)

def neighborsCnt(walls: Set[Pos])(p: PosInt): Set[PosInt] =
  val ps = p._1.near4.map(convertCoord(g)) -- walls
  ps.map(_ -> p._2)

neighborsCnt(walls)(start -> 1)

def visitStepsCnt(cntStep: Int, start: Pos, neighbors: PosInt => Set[PosInt]): Set[PosInt] =
  @tailrec
  def visited(cntStep: Int, toVisit: Set[PosInt]): Set[PosInt] =
    if cntStep == 0 then toVisit
    else

      visited(cntStep - 1, toVisit.flatMap(neighbors) -- toVisit)
  visited(cntStep, Set(start -> 1))

