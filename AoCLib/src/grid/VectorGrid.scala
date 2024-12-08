package grid

import common.Default
import common.Default.default
import coord.{Dir, Pos, given}
import box.Box
import exts.*
import grid.Direction.*
import parse.Read

import scala.util.matching.Regex

class VectorGrid[A : Default] private (protected val grid: Vector[Vector[A]]) extends Grid[A]:
  override lazy val minPos: Pos = Pos.zero
  override lazy val maxPos: Pos = Pos(grid.head.length-1, grid.length-1)

  override lazy val gridBox: Box = Box(minPos, maxPos)

  override def mkString(mapf: A => String = _.toString)
                       (firstRow: String = "",
                        rowSep: String = "\n",
                        lastRow: String = "",
                        firstPos: String = "",
                        posSep: String = "",
                        lastPos: String = ""
                        ): String =
    grid.map(_.map(mapf))
      .map(_.mkString(firstPos, posSep, lastPos))
      .mkString(firstRow, rowSep, lastRow)

  override def apply(p: Pos): A = grid(p.y)(p.x)
  override def get(p: Pos): Option[A] = Option.when(contains(p))(apply(p))
  override def getOrElse(p: Pos, orElse: A): A = if contains(p) then apply(p) else orElse

  override def map[B](f: A => B): Grid[B] =
    given Default[B] = f(default[A])
    new VectorGrid(grid.map(_.map(f)))
  override def mapWithPos[B](f: (Pos, A) => B): Grid[B] =
    given Default[B] = f(Pos.zero, default[A])
    new VectorGrid(grid.zipWithIndex.map((l,y) => l.zipWithIndex.map((a,x) => f(Pos(y,x), a))))
  override def mapToPos[B](f: (Pos, A) => (Pos, B)): Grid[B] =
    given Default[B] = f(Pos.zero, default[A])._2
    MapGrid(grid.zipWithIndex.flatMap((l,y) => l.zipWithIndex.map((a,x) => f(Pos(y,x), a))).toMap)

  override def updated(p: Pos, a: A): Grid[A] =
    if contains(p)
    then new VectorGrid(grid.updated(p.y, grid(p.y).updated(p.x, a)))
    else this
  override def transpose: Grid[A] = new VectorGrid(grid.transpose)
  override def rotateCW(cnt: Int = 1): Grid[A] =
    new VectorGrid(cols.map(_.toVector.reverse).toVector)

  override def values: Seq[A] = grid.flatMap(_.toSeq)
  override def count(f: A => Boolean): Int = grid.map(_.count(f)).sum
  override def sum(using Numeric[A]): A = grid.iterator.map(_.sum).sum
  override def allPos: Set[Pos] = gridBox.iterator.toSet

  override def find(fp: (Pos, A) => Boolean): Option[Pos] =
    grid.zipWithIndex
        .map((r, y) => r.zipWithIndex
                        .collectFirst {
                          case (c, x) if fp(Pos(x, y), c) => Pos(x, y) })
        .collectFirst { case Some(p) => p }
  override def filter(f: A => Boolean): Grid[A] = MapGrid(iterator.filter((_,a) => f(a)).toMap)
  override def filter(ff: (Pos, A) => Boolean): Grid[A] = MapGrid(iterator.filter(ff.tupled).toMap)

  override def row(r: Int): Seq[A] =
    if r >= minPos.y && r <= maxPos.y
    then grid(r)
    else Seq.empty
  override def col(c: Int): Seq[A] =
    if c >= minPos.x && c <= maxPos.x
    then grid.map(r => r(c))
    else Seq.empty

  private def checkRange(seq: Direction)(i: Int): Boolean =
    seq match
      case Row => i >= minPos.y && i <= maxPos.y
      case Column => i >= minPos.x && i <= maxPos.x

  override def remove(i: Int, seq: Direction): Grid[A] =
    if !checkRange(seq)(i)
    then this
    else seq match
      case Row =>
        val (u,d) = grid.splitAt(i)
        new VectorGrid(u ++ d.tail)
      case Column =>
        new VectorGrid(grid.map(row =>
          val (l,r) = row.splitAt(i)
          l ++ r.tail))

  override def clear(i: Int, seq: Direction): Grid[A] =
    if !checkRange(seq)(i)
    then this
    else seq match
      case Row =>
        new VectorGrid(grid.updated(i, Vector.fill(grid(i).size)(default[A])))
      case Column =>
        new VectorGrid((minPos.y to maxPos.y)
          .foldLeft(grid){(g,r) => g.updated(r, g(r).updated(i, default[A]))})

  override def rows: Iterator[Seq[A]] = grid.iterator
  override def cols: Iterator[Seq[A]] = grid.transpose.iterator

  override def iterator: Iterator[(Pos, A)] =
    gridBox.iterator.map(p => p -> this(p))
      .filterNot(_._2 == default[A])
  override def iteratorAll: Iterator[(Pos, A)] = gridBox.iterator.map(p => p -> apply(p))

  override def toMapGrid: MapGrid[A] = MapGrid(this.iterator.toMap)
  override def toVectorGrid: VectorGrid[A] = this

  override def --(xs: Set[Pos]): Grid[A] = toMapGrid -- xs
  override def +(cell: (Pos, A)): Grid[A] = toMapGrid + cell
  override def keepOnlyInPositions(positions: Set[Pos]): Grid[A] = toMapGrid.keepOnlyInPositions(positions)

  override def equals(other: Any): Boolean = grid == other.asInstanceOf[VectorGrid[A]].grid
  override def hashCode: Int = grid.##

object VectorGrid:
  def apply[A : Default](g: Vector[Vector[A]]): VectorGrid[A] = new VectorGrid(g)

  def apply[A : Default](mpa: Map[Pos, A]): VectorGrid[A] =
    new VectorGrid(MapGrid(mpa)
      .iteratorAll.toSeq
      .groupMap(_._1.y)(_._2).values
      .map(_.toVector).toVector)

  def apply(s: String)(using Default[Char]): CharGrid =
    val ch = default[Char]
    apply(s.asStrs.map(_.toCharArray.toVector).toVector)

//  extension [A: Default](g: VectorGrid[A])
//    def flattenGrid[B](using asGrid: A => Grid[B]): Grid[B] =
//      VectorGrid(g.map(asGrid).toVectorGrid.grid.flatMap(_.transpose.map(_.flatten)))
//
//  def groupedGrid(groupSize: Int): Grid[Grid[A]] =
//    grid.grouped(groupSize).map(_.map(_.grouped(groupSize).toVector).transpose).toVector
//
//  def slidingGrid(size: Pos): Iterator[Iterator[Grid[A]]] =
//    grid.sliding(size.y).map(_.map(_.sliding(size.x).toVector).transpose.iterator)
//
//  def slidingGrid(size: Int): Iterator[Iterator[Grid[A]]] = slidingGrid(Pos(size, size))
//
//  def correspondsGrid[B](otherGrid: Grid[B])(p: (A, B) => Boolean): Boolean =
//    grid.corresponds(otherGrid)(_.corresponds(_)(p))
//
//  def sizeGrid: Int = grid.size * grid.head.size

  given charVectorGridReader: Read[CharGrid] = VectorGrid.apply(_)
