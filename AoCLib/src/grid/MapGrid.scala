package grid

import common.Default
import coord.{Dir, Pos, given}
import coord.Dir.*
import box.Box
import common.Default.default
import exts.*
import grid.Direction.{Column, Row}
import parse.Read

import scala.util.matching.Regex

class MapGrid[A : Default] private(protected val cells: Map[Pos, A]) extends Grid[A]:
  private lazy val (minCol, minRow, maxCol, maxRow) =
    val Pos(x0, y0) = cells.keySet.head
    cells.keys.foldLeft(x0, y0, x0, y0):
      case (xmi, ymi, xma, yma) -> p =>
        (xmi min p.x, ymi min p.y, xma max p.x, yma max p.y)
  override lazy val gridBox: Box = Box(Pos(minCol, minRow), Pos(maxCol, maxRow))
//  lazy val minRow: Int = cells.keys.map(_.y).min
//  lazy val maxRow: Int = cells.keys.map(_.y).max
//  lazy val minCol: Int = cells.keys.map(_.x).min
//  lazy val maxCol: Int = cells.keys.map(_.x).max

  override def mkString(mapf: A => String = _.toString)
                       (firstRow: String = "",
                        rowSep: String = "\n",
                        lastRow: String = "",
                        firstPos: String = "",
                        posSep: String = "",
                        lastPos: String = ""
                        ): String =
    (minRow to maxRow)
      .map { row =>
        (minCol to maxCol).map { col =>
          mapf(cells.getOrElse(Pos(col, row), default[A]))
        }.mkString(firstPos,posSep,lastPos)
      }.mkString(firstRow,rowSep,lastRow)

  def debug: Map[Pos, A] = cells

  override def apply(pos: Pos): A = cells(pos)
  override def get(pos: Pos): Option[A] = cells.get(pos)
  override def getOrElse(pos: Pos, orElse: A): A = cells.getOrElse(pos, orElse)

  override def map[B](f: A => B): Grid[B] =
    given Default[B] = f(default[A])
    new MapGrid(cells.view.mapValues(f).toMap)
  override def mapWithPos[B](f: (Pos, A) => B): Grid[B] =
    given Default[B] = f(Pos.zero, default[A])
    new MapGrid(cells.view.map((p,v) => p -> f(p,v)).toMap)
  override def mapToPos[B](f: (Pos, A) => (Pos, B)): Grid[B] =
    given Default[B] = f(Pos.zero, default[A])._2
    new MapGrid(cells.view.map(f.tupled).toMap)

  override def updated(p: Pos, c: A): Grid[A] =
    if contains(p)
    then new MapGrid(cells.updated(p, c))
    else this
  override def transpose: Grid[A] = new MapGrid(cells.map((p, c) => p.swap -> c))
  override def rotateCW(cnt: Int = 1): Grid[A] = mapToPos((p,ch) => p.rotateTimes(cnt) -> ch)

  override def values: Seq[A] = cells.values.toSeq
  override def count(p: A => Boolean): Int = cells.valuesIterator.count(p)
  override def sum(using Numeric[A]): A = cells.values.sum
  override def allPos: Set[Pos] = cells.keySet

  override def find(fp: (Pos, A) => Boolean): Option[Pos] = cells.find(fp.tupled).map(_._1)
  override def findAll(a: A): Iterator[Pos] = cells.filter(_._2 == a).keysIterator
  override def filter(p: A => Boolean): Grid[A] =
    new MapGrid(cells.filter((_, char) => p(char)))
  override def filter(ff: (Pos, A) => Boolean): Grid[A] =
    new MapGrid(cells.filter(ff.tupled))

  override def row(r: Int): Seq[A] =
    if minRow <= r && r <= maxRow
    then (minCol to maxCol).map(col => cells.getOrElse(Pos(col, r), default[A]))
    else Seq.empty
  override def col(c: Int): Seq[A] =
    if minCol <= c && c <= maxCol
    then (minRow to maxRow).map(row => cells.getOrElse(Pos(c, row), default[A]))
    else Seq.empty

  override def view(seq: Direction)(i: Int): Seq[A] =
    if seq == Row then row(i) else col(i)

  override def remove(seq: Direction)(i: Int): Grid[A] =
    val (fp, td) = seq match
      case Row    => ((p: Pos) => p.y, N)
      case Column => ((p: Pos) => p.x, W)
    val (l,r) = cells.partition((p,_) => fp(p) <= i)
    new MapGrid(l.filterNot((p,_) => fp(p) == i) ++ r.map((p,v) => p.toDir(td) -> v))

  override def clear(seq: Direction)(i: Int): Grid[A] =
    val ff: Pos => Int = seq match
      case Row    => _.y
      case Column => _.x
    new MapGrid(cells.filterNot((p,_) => ff(p) == i))

  override def iterator: Iterator[(Pos, A)] = cells.iterator
  override def iteratorAll: Iterator[(Pos, A)] =
    gridBox.iterator.map(p => p -> cells.getOrElse(p, default[A]))

  override def toVectorGrid: VectorGrid[A] = VectorGrid(iteratorAll.toMap)
  override def toMapGrid: MapGrid[A] = this

//  override def transform(m: Matrix[3, 3, Int]): Grid[A] =
//    new MapGrid(cells.map((pos, char) => (Pos.fromAffine(m * pos.toAffine), char)))
  override def --(xs: Set[Pos]): Grid[A] =
    new MapGrid(cells -- xs)
  override def -(cell: Pos): Grid[A] =
    new MapGrid(cells -- Set(cell))
  override def ++(xs: Set[(Pos, A)]): Grid[A] =
    new MapGrid(cells ++ xs)
  override def +(cell: (Pos, A)): Grid[A] =
    new MapGrid(cells + cell)  
  override def keepOnlyInPositions(positions: Set[Pos]): Grid[A] =
    new MapGrid(allPos.intersect(positions).map(p => p -> cells(p)).toMap)
  //new MapGrid(g.cells.filter((pos, _) => positions.contains(pos)))  
  
  override def equals(other: Any): Boolean = cells == other.asInstanceOf[MapGrid[A]].cells
  override def hashCode: Int = cells.##

object MapGrid:
  def apply[A : Default](input: Seq[Seq[A]]): MapGrid[A] =
    val cells =
      (for
        (line, y) <- input.zipWithIndex
        (char, x) <- line.zipWithIndex
        if char != default[A]
      yield Pos(x, y) -> char)
        .toMap
//      input.zipWithIndex.flatMap { (line, y) =>
//      line.zipWithIndex.map { (char, x) =>
//        Pos(x, y) -> char
//      }
//    }.toMap
    apply(cells)

  def apply(input: String)(using Default[Char]): MapGrid[Char] =
    val ch = default[Char]
    apply(input.asStrs.map(_.toCharArray.toSeq))

  def apply[A: Default](cells: Map[Pos, A]): MapGrid[A] =
    new MapGrid(cells)

  def empty[A: Default]: MapGrid[A] = new MapGrid(Map.empty)

//  class Group[A](val firstPos: Pos, val length: Int, val res: Seq[A]):
//    def allPos: Set[Pos] =
//      (firstPos.x until (firstPos.x + length)).toSet.map(col => Pos(col, firstPos.y))
//
//    def neighbors: Set[Pos] =
//      allPos.flatMap(_.nearAll) -- allPos
//
//  extension [A: Default] (g: MapGrid[A])
//    def &(other: MapGrid[A]): Grid[A] =
//      val commonPos = g.cells.keySet & other.cells.keySet
//      new MapGrid(
//        commonPos.filter(pos => g.cells(pos) == other.cells(pos)).map(pos => (pos, g.cells(pos))).toMap
//      )
//
//    def findAll(length: Int, regex: Regex): Set[Pos] =
//      g.cells.keySet.flatMap { pos =>
//        val sequence = (pos.x until (pos.x + length)).map(col => g.cells.get(Pos(col, pos.y)))
//        if sequence.forall(_.isDefined) then
//          val string = sequence.flatten.mkString
//          Option.when(regex.matches(string))(pos)
//        else None
//      }
//
//    def findAllVertical(length: Int, regex: Regex): Set[Pos] =
//      g.cells.keySet.flatMap { pos =>
//        val sequence = (pos.y until (pos.y + length)).map(row => g.cells.get(Pos(pos.x, row)))
//        if sequence.forall(_.isDefined) then
//          val string = sequence.flatten.mkString
//          Option.when(regex.matches(string))(pos)
//        else None
//      }
//
//    def findGroups(p: A => Boolean): List[Group[A]] =
//      g.cells
//        .filter((pos, char) => p(char) && (!g.cells.contains(pos.toDir(W)) || !p(g.apply(pos.toDir(W)))))
//        .keys
//        .map { pos =>
//          val lastPos = Iterator
//            .iterate(pos)(_.toDir(E))
//            .takeWhile(g.cells.contains)
//            .takeWhile(pos => p(g.apply(pos)))
//            .toList
//            .last
//          val length  = lastPos.x - pos.x + 1
//          val res     = (pos.x until (pos.x + length)).map(col => g.apply(Pos(col, pos.y)))
//          new Group(pos, length, res)
//        }
//        .toList


//    def aStar(goal: Pos, obstacle: (Pos, A) => Boolean)(using Neighbors): AStarExt[Pos, Int] =
//      new AStarExt[Pos, Int](
//        _ == goal,
//        _.manhattan(goal),
//        (_, _) => 1,
//        0,
//        _.neighbors.filter(g.gridBox.contains).filterNot(pos => obstacle(pos, g.cells(pos)))
//      )
//
//    def bfs(start: Pos, filter: (Pos, Char) => Boolean = (_,_) => true)(using Neighbors): Set[Pos] =
//      BFSExt(start, summon[Neighbors].neighbors.andThen(_.filter(g.gridBox.contains)))

  given charMapGridReader: Read[CharGrid] with
    override def read(input: String): CharGrid = MapGrid(input)
