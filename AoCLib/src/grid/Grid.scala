package grid

import common.{Default, given}
import common.Default.default
import coord.{Neighbor, Pos, given}
import box.Box
import graph.{ DFS, BFS, AStar }
import Direction.*

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

  def map[B](f: A => B): Grid[B]
  def mapWithPos[B](f: (Pos, A) => B): Grid[B]
  def mapToPos[B](f: (Pos, A) => (Pos, B)): Grid[B]

  def updated(p: Pos, a: A): Grid[A]
  def transpose: Grid[A]
  def rotateCW(cnt: Int = 1): Grid[A]

  def values: Seq[A]
  def count(a: A => Boolean): Int
  def neighborCount(pos: Pos, p: A => Boolean)(using N: Neighbor[A]): Int =
    N.neighbors(pos, apply(pos)).toList.map(get.andThen(_.map(p))).count(_.getOrElse(false))
  def sum(using Numeric[A]): A
  def allPos: Set[Pos]

  def find(a: A): Option[Pos] = find((_, c) => c == a)
  def find(a: A => Boolean): Option[Pos] = find((_, c) => a(c))
  def find(a: (Pos, A) => Boolean): Option[Pos]
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
  
  def dfs(start: Pos)(using N: Neighbor[A]): (Map[Pos, Int], Seq[Pos]) =
    DFS.traverse(start, p => N.neighbors(p, apply(p)).filter(gridBox.contains))

  def bfs(start: Pos)(using N: Neighbor[A]): Map[Pos, Int] =
    BFS.traverse(start, p => N.neighbors(p, apply(p)).filter(gridBox.contains))
