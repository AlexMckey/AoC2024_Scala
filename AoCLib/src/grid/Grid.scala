package grid

import common.{Default, given}
import coord.{Dir, Neighbors, Pos, given}
import box.Box
import Direction.*
import struct.VTree
import traverse.other.AStar
import traverse.{AStar, BFS, DFS}

import scala.util.matching.Regex

enum Direction:
  case Row, Column

type CharGrid = Grid[Char]

abstract class Grid[A : Default] derives CanEqual:
  lazy val minPos: Pos = gridBox.dl
  lazy val maxPos: Pos = gridBox.ur
  lazy val gridBox: Box
  def contains(p: Pos): Boolean = gridBox.contains(p)

  def mkString(mapf: A => String = _.toString)
              (firstRow: String = "",
               rowSep: String = "\n",
               lastRow: String = "",
               firstPos: String = "",
               posSep: String = "",
               lastPos: String = ""
              ): String

  override def toString: String = mkString()()

  def apply(p: Pos): A
  def get(p: Pos): Option[A]
  def getOrElse(p: Pos, orElse: A): A
  def isEq(p: Pos, a: A): Boolean = get(p).contains(a)

  def map[B](f: A => B): Grid[B]
  def mapWithPos[B](f: (Pos, A) => B): Grid[B]
  def mapToPos[B](f: (Pos, A) => (Pos, B)): Grid[B]

  def updated(p: Pos, a: A): Grid[A]
  def transpose: Grid[A]
  def rotateCW(cnt: Int = 1): Grid[A]

  def values: Seq[A]
  def count(a: A => Boolean): Int
  def neighborCount(pos: Pos, p: A => Boolean)(using N: Neighbors[A]): Int =
    N.neighbors(pos, apply(pos)).toList.map(get.andThen(_.map(p))).count(_.getOrElse(false))
  def sum(using Numeric[A]): A
  def allPos: Set[Pos]

  def find(a: A): Option[Pos] = find((_, c) => c == a)
  def find(a: A => Boolean): Option[Pos] = find((_, c) => a(c))
  def find(a: (Pos, A) => Boolean): Option[Pos]
  def findAll(length: Int, regex: Regex, directions: List[Dir] = List(Dir.E)): Iterator[Pos] =
    for
      pos <- allPos.iterator
      direction <- directions.iterator
      string = Iterator.iterate(pos)(_ + direction.delta).take(length).flatMap(get).mkString
      if string.length == length && regex.matches(string)
    yield pos
  def findAll(a: A): Iterator[Pos]  
  def filter(f: A => Boolean): Grid[A]
  def filter(ff: (Pos, A) => Boolean): Grid[A]

  def row(r: Int): Seq[A]
  def col(c: Int): Seq[A]
  def rows: Iterator[Seq[A]] =
    (gridBox.min.y to gridBox.max.y).iterator.map(row)
  def cols: Iterator[Seq[A]] =
    (gridBox.min.x to gridBox.max.x).iterator.map(col)
  def remove(i: Int, seq: Direction): Grid[A]
  def clear(r: Int, seq: Direction): Grid[A]

  def iterator: Iterator[(Pos, A)]
  def iteratorAll: Iterator[(Pos, A)]
  
  def toMapGrid: MapGrid[A]
  def toVectorGrid: VectorGrid[A]

//  def transform(m: Matrix[3, 3, Int]): Grid[A]
  def --(xs: Set[Pos]): Grid[A]
  def +(cell: (Pos, A)): Grid[A]
  def keepOnlyInPositions(positions: Set[Pos]): Grid[A]
  
  def dfs(start: Pos)(using N: Neighbors[A]): (Map[Pos, Int], Seq[Pos]) =
    DFS.traverse(start, p => N.neighbors(p, apply(p)).filter(gridBox.contains))

  def bfs(start: Pos)(using N: Neighbors[A]): Map[Pos, Int] =
    BFS.traverse(start, p => N.neighbors(p, apply(p)).filter(gridBox.contains))

  def vTree(obstacle: A => Boolean)(using N: Neighbors[A]): VTree[Pos] = new VTree[Pos]:
    override def children(node: Pos, visited: Set[Pos]): Iterator[Pos] =
      val allNeighbors = N.neighbors(node, apply(node)).filter(contains).filterNot(pos => obstacle(apply(pos)))
      allNeighbors.filterNot(visited.contains).iterator

  def aStar(goal: Pos, obstacle: A => Boolean)(using N: Neighbors[A]): AStar[Pos, Int] =
    new AStar[Pos, Int](_ == goal, _.manhattan(goal), (_, _) => 1, 0, p => N.neighbors(p, apply(p)).toSet.filter(contains).filterNot(pos => obstacle(apply(pos))))
