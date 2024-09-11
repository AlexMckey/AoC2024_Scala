package graph

import scala.collection.mutable

object Dijkstra:

  def traverse[A](start: A,
                  neighbors: A => IterableOnce[(A,Int)]
                 ): Map[A, Int] =
    val visitedDistance: mutable.Map[A, Int] = mutable.Map.empty
    val toVisit: mutable.PriorityQueue[(Int, A)] = mutable.PriorityQueue.empty(Ordering.by(-_._1))

    def enqueue(node: A, dist: Int): Unit = toVisit.enqueue(dist -> node)

    enqueue(start, 0)

    while toVisit.nonEmpty do
      val (dist, node) = toVisit.dequeue()
      if !visitedDistance.contains(node)
      then
        visitedDistance(node) = dist

        def goNeighbor(newNode: A, distDelta: Int): Unit =
          if !visitedDistance.contains(newNode)
          then enqueue(newNode, dist + distDelta)

        neighbors(node).iterator.foreach((goNeighbor _).tupled)
    end while

    visitedDistance.toMap
  end traverse

  def search[A](start: A, isTarget: A => Boolean, neighbors: A => IterableOnce[(A, Int)]): Option[(A, Int)] =
    search(start, (t,_) => isTarget(t), neighbors)

  def search[A](start: A, target: A, neighbors: A => IterableOnce[(A, Int)]): Option[(A, Int)] =
    search(start, _ == target, neighbors)

  def search[A](start: A,
                isTarget: (A, Int) => Boolean,
                neighbors: A => IterableOnce[(A, Int)]
               ): Option[(A, Int)] =
    val visitedDistance: mutable.Map[A, Int] = mutable.Map.empty
    val toVisit: mutable.PriorityQueue[(Int, A)] = mutable.PriorityQueue.empty(Ordering.by(-_._1))

    def enqueue(node: A, dist: Int): Unit = toVisit.enqueue((dist, node))

    enqueue(start, 0)

    while (toVisit.nonEmpty)
      val (dist, node) = toVisit.dequeue()
      if !visitedDistance.contains(node)
      then
        visitedDistance(node) = dist

        if isTarget(node, dist)
        then return Some(node -> dist)

        def goNeighbor(newNode: A, distDelta: Int): Unit =
          if !visitedDistance.contains(newNode)
          then enqueue(newNode, dist + distDelta)

        neighbors(node).iterator.foreach(goNeighbor)
    end while

    None
  end search
