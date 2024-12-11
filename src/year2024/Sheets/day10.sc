import coord.{Neighbor, Pos}
import coord.given
import graph.BFS
import grid.VectorGrid

import scala.collection.mutable

val s = "89010123\n78121874\n87430965\n96549874\n45678903\n32019012\n01329801\n10456732"
//val s = "012345\n123456\n234567\n345678\n4.6789\n56789."
val g = VectorGrid(s)
val starts = g.filter(_ == '0').allPos.toList
given gn: Neighbor[Char] with
  override def neighbors(p: Pos, a: Char): Iterator[Pos] =
    p.nearAxis.filter(g.contains).filter(n => g(n) - g(p) == 1)
starts.map(s => g.bfs(s).count(_._2 == 9)).sum

g.bfs(starts(0)).groupMap(_._2)(_._1).toList.sortBy(_._1)
g.bfs(starts(0)).keysIterator.toList.groupMapReduce(identity)(_ => 1)(_ + _)
starts.map(s => g.bfs(s).groupMapReduce(_._2)(_ => 1)(_ + _).values.product).sum

def ns(p: Pos): Iterator[Pos] =
  p.nearAxis.filter(g.contains).filter(n => g(n) - g(p) == 1)

BFS.components(starts.toSet, ns).map(_.size)

def traverse[A](start: A, neighbors: A => IterableOnce[A]): Map[A, Int] =
  val visitedCnt: mutable.Map[A, Int] = mutable.Map.empty.withDefaultValue(0)
  val toVisit: mutable.Queue[A]     = mutable.Queue.empty
  toVisit.enqueue(start)
  while toVisit.nonEmpty do
    val node = toVisit.dequeue()
    visitedCnt(node) += 1
    toVisit.appendAll(neighbors(node))
  end while
  visitedCnt.toMap
end traverse

ns(starts(0)).toList
val all9 = g.filter(_ == '9').allPos
val res: Seq[Map[Pos, Int]] = starts.map(st => traverse(st, ns).view.filterKeys(all9.contains).toMap)
res.map(_.keySet.size).sum
res.map(_.values.sum).sum
