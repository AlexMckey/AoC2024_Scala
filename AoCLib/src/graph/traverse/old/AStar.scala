package graph.traverse.old

import scala.collection.mutable

object AStar:

  def search[A](start: A, target: A, neighbors: A => IterableOnce[A], heuristic: A => Int): (Option[(A, Int)], Map[A, Int]) =
    search(start, _ == target, neighbors, heuristic)

  def search[A](start: A, isTarget: A => Boolean, neighbors: A => IterableOnce[A], heuristic: A => Int): (Option[(A, Int)], Map[A, Int]) =
    search(start, (t,_) => isTarget(t), neighbors(_).iterator.map(_ -> 1), heuristic)
  
  def search[A](start: A,
                isTarget: (A, Int) => Boolean,
                neighbors: A => IterableOnce[(A, Int)],
                heuristic: A => Int
               ): (Option[(A, Int)], Map[A, Int]) =
    val visitedDistance: mutable.Map[A, Int] = mutable.Map.empty
    val toVisit: mutable.PriorityQueue[(Int, Int, A)] = mutable.PriorityQueue.empty(Ordering.by(-_._1))

    def enqueueHeuristically(node: A, dist: Int): Unit =
      toVisit.enqueue((dist + heuristic(node), dist, node))

    enqueueHeuristically(start, 0)

    while toVisit.nonEmpty do
      val (_, dist, node) = toVisit.dequeue()
      if !visitedDistance.contains(node)
      then
        visitedDistance(node) = dist

        if isTarget(node, dist)
        then return Some(node -> dist) -> visitedDistance.toMap


        def goNeighbor(newNode: A, distDelta: Int): Unit =
          if !visitedDistance.contains(newNode)
          then enqueueHeuristically(newNode, dist + distDelta)

        neighbors(node).iterator.foreach(goNeighbor)
    end while

    None -> visitedDistance.toMap
  end search