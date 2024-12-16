import box.Box
import common.Default
import coord.Dir.axisDirs
import coord.{Coord, Dir, Neighbors, Pos, axisNeighbors, pos}
import grid.{CharGrid, MapGrid, VectorGrid}
import exts.iterables.groupCount
import graph.traverse.old.BFS

import scala.collection.mutable

//val s = "AAAA\nBBCD\nBBCC\nEEEC"
//val s = "OOOOO\nOXOXO\nOOOOO\nOXOXO\nOOOOO"
//val s = "EEEEE\nEXXXX\nEEEEE\nEXXXX\nEEEEE"
val s = "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE"
//val s = "AAAAAA\nAAABBA\nAAABBA\nABBAAA\nABBAAA\nAAAAAA"
val g = VectorGrid(s)

val ps = g.allPos

def neighbors(g: CharGrid)(p: Pos): Iterator[Pos] =
  p.nearAxis.filter(g.contains).filter(g(_) == g(p))

//case class RegionNeighbors(g: CharGrid) extends Neighbors[Char]:
//  override def neighbors(p: Pos, a: Char): Iterator[Pos] =
//    p.nearAll.filter(g.contains).filter(g(_) == a)
//
//val p1 = ps.head
//g(p1)
////n.neighbors(p1, g(p1)).toList
//g.vTree(_ => false).allReachable(p1, p => g.contains(p) && g(p) == g(p1))
//
//def allRegions(positions: Set[Pos], acc: List[Set[Pos]] = List.empty): List[Set[Pos]] =
//  if positions.isEmpty then acc
//  else
//    val pos = positions.head
//    val r = g.vTree(_ => false).allReachable(pos, p => g.contains(p) && g(p) == g(pos))
//    allRegions(positions -- r, r +: acc)
//
//allRegions(g.allPos)

def traverseRegion(start: Pos, neighbors: Pos => IterableOnce[Pos]): Map[Pos, Int] =
  val visited: mutable.Map[Pos, Int] = mutable.Map.empty.withDefaultValue(4)
  val toVisit: mutable.Queue[Pos] = mutable.Queue.empty

  toVisit.enqueue(start)

  while toVisit.nonEmpty do
    val node = toVisit.dequeue()
    if !visited.contains(node)
    then
      val ns = neighbors(node).iterator.toSet
      visited(node) -= ns.size
      toVisit.enqueueAll(ns)
  end while

  visited.toMap
end traverseRegion

def region(start: Pos, neighbors: Pos => IterableOnce[Pos]): Set[Pos] =
  val visited: mutable.Set[Pos] = mutable.Set.empty
  val toVisit: mutable.Queue[Pos] = mutable.Queue.empty

  toVisit.enqueue(start)

  while toVisit.nonEmpty do
    val node = toVisit.dequeue()
    if !visited.contains(node)
    then
      val ns = neighbors(node).iterator.toSet
      visited.add(node)
      toVisit.enqueueAll(ns)
  end while

  visited.toSet
end region

traverseRegion(ps.tail.head, neighbors(g))
val r1 = region(Pos(1,1), neighbors(g))
r1.size

r1.iterator // iterator to avoid deduplicating same edge counts
  .map(pos => 4 - pos.nearAxis.count(r1))
  .sum

val p1 = r1.toList
  .flatMap(p => Set(p.toDir(Dir.N) -> p, p.toDir(Dir.W) -> p, p -> p.toDir(Dir.E), p -> p.toDir(Dir.S)))
  .groupCount(identity)
  .toList
  .filter(_._2 == 1)
  .map(_._1)
p1.size

def groupsCount(xs: Seq[Int]): Int =
  1 + xs.sorted.sliding(2).count:
    case Seq(l, r) => r - l > 1
    case Seq(only) => false

val sides: Int =
  val horizontal = p1.filter(_.x == _.x).map(_._1).groupMap(pos => (pos.y, r1.contains(pos)))(_.x)
  val vertical = p1.filter(_.y == _.y).map(_._1).groupMap(pos => (pos.x, r1.contains(pos)))(_.y)
  vertical.values.map(groupsCount).sum + horizontal.values.map(groupsCount).sum

def regions(g: CharGrid): List[(Char, Int, Int)] =
  def rec(ps: Set[Pos], acc: List[(Char, Int, Int)] = List.empty): List[(Char, Int, Int)] =
    if ps.isEmpty then acc
    else
      val p = ps.head
      val res = traverseRegion(p, neighbors(g))
      rec(ps -- res.keySet, (g(p), res.size, res.values.sum) +: acc)

  rec(g.allPos)

regions(g)
  .map((_, area, perimeter) => area * perimeter).sum

import Dir.*
val cs = BFS.components(ps, neighbors(g)).toList
cs.map(_.size)
val csr = cs(2)
val (pr1, pr2) = Coord.boundingBox(csr)
val b = Box(pr1.toDir(NW), pr2)
val pss = b.iterator.map(p => List(p, p.toDir(S), p.toDir(E), p.toDir(SE))).toList
pss.map(_.map(g.getOrElse(_, '.')))

pss.map(_.map{p => if csr.contains(p) && g(csr.head) == g.getOrElse(p, '.') then 1 else 0 }
  .zip(List(1,-1,-1,1))
  .map(_ * _)
  .sum.abs).sum

def countSides(r: Set[Pos]): Int =
  val (p1, p2) = Coord.boundingBox(r)
  val b = Box(p1.toDir(NW), p2)
  val ps = b.iterator.map(p => List(p, p.toDir(S), p.toDir(E), p.toDir(SE)))
  ps.map(_.map { p => if r.contains(p) then 1 else 0 }
    .zip(List(1, -1, -1, 1))
    .map(_ * _)
    .sum.abs).sum

cs.map(countSides).zip(cs.map(_.size)).map(_ * _).sum