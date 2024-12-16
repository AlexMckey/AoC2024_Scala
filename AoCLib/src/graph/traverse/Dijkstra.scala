package graph.traverse

import scala.collection.mutable

object Dijkstra:
  def search[A](start: A, goal: A => Boolean)(ns: A => Set[(A, Int)]): (Map[A, Int], Option[(A, Int)]) =
    val distances = mutable.Map[A, Int](start -> 0)
    val unseen = mutable.PriorityQueue((0, start))(Ordering.by(-_._1))
    val visited = mutable.Set.empty[A]
    while unseen.nonEmpty do
      val (currentDist, currentNode) = unseen.dequeue()
      if !visited.contains(currentNode) then
        visited.add(currentNode)
        if goal(currentNode) then return (distances.toMap, Some(currentNode -> currentDist))
        // Process neighbors
        for (neighbor, weight) <- ns(currentNode) do
          val newDist = currentDist + weight
          if newDist < distances.getOrElse(neighbor, Int.MaxValue) then
            distances(neighbor) = newDist
            unseen.enqueue((newDist, neighbor))

    (distances.toMap, None)