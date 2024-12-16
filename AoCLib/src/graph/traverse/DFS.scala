package graph.traverse

import scala.collection.mutable

object DFS:
  def apply[A](start: A)(ns: A => Iterable[A]): Iterable[A] =
    def rec(cur: A, visited: Iterable[A] = Iterable.empty): Iterable[A] =
      if visited.iterator.contains(cur) then visited
      else
        val neighbors = ns(cur).filterNot(visited.iterator.contains)
        neighbors.foldLeft(Iterable(cur) ++ visited)((b, a) => rec(a, b))

    rec(start)
  end apply
  
  def search[A](start: A, goal: A => Boolean)(next: A => Seq[A]): Seq[Seq[A]] =
    def rec(p: A, path: Seq[A], visited: Set[A]): Seq[Seq[A]] =
      if goal(p) then Seq(path)
      else
        val neighbors = next(p).filterNot(visited.contains)
        neighbors.flatMap(n => rec(n, path :+ n, visited + n))

    rec(start, Seq(start), Set(start))
  end search

  def traverse[A](start: A)(ns: A => Iterable[A]): (Map[A, Int], Seq[A]) =
    val visitedDistance: mutable.Map[A, Int] = mutable.Map.empty
    val visitedOrder: mutable.Buffer[A] = mutable.Buffer.empty
    val toVisit: mutable.Stack[(Int, A)] = mutable.Stack.empty

    inline def enqueue(node: A, dist: Int): Unit = toVisit.push(dist -> node)

    enqueue(start, 0)

    while toVisit.nonEmpty do
      val (dist, node) = toVisit.pop()
      if !visitedDistance.contains(node)
      then
        visitedOrder += node
        visitedDistance(node) = dist

        inline def goNeighbor(newNode: A): Unit = enqueue(newNode, dist + 1)

        ns(node).iterator.foreach(goNeighbor)
    end while

    visitedDistance.toMap -> visitedOrder.toSeq
  end traverse
