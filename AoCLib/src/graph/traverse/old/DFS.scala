package graph.traverse.old

import scala.collection.mutable

object DFS:

  def traverse[A](start: A,
                       neighbors: A => IterableOnce[A]
                      ): (Map[A, Int], Seq[A]) =
    val visitedDistance: mutable.Map[A, Int] = mutable.Map.empty
    val visitedOrder: mutable.Buffer[A] = mutable.Buffer.empty
    val toVisit: mutable.Stack[(Int, A)] = mutable.Stack.empty

    def enqueue(node: A, dist: Int): Unit = toVisit.push(dist -> node)

    enqueue(start, 0)

    while toVisit.nonEmpty do
      val (dist, node) = toVisit.pop()
      if !visitedDistance.contains(node)
      then  
        visitedOrder += node
        visitedDistance(node) = dist

        def goNeighbor(newNode: A): Unit = enqueue(newNode, dist + 1)

        neighbors(node).iterator.foreach(goNeighbor)
    end while

    visitedDistance.toMap -> visitedOrder.toSeq
  end traverse  