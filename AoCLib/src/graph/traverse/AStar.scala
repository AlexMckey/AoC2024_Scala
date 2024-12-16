package graph.traverse

import scala.collection.mutable

object AStar:
  def apply[A](start: A, goal: A)(ns: A => Set[(A, Int)])(heuristic: A => Long): Option[(Long, Map[A, Long])] =
    case class Node(point: A, cost: Long, estimatedTotalCost: Long)
    val priorityQueue = mutable.PriorityQueue.empty[Node](Ordering.by(-_.estimatedTotalCost))
    priorityQueue.enqueue(Node(start, 0, heuristic(start)))

    val visited = collection.mutable.Set.empty[A]
    val bestCosts = mutable.Map[A, Long](start -> 0)

    while (priorityQueue.nonEmpty)
      val current = priorityQueue.dequeue()
      if current.point == goal then return Some(current.cost, bestCosts.toMap)
      ns(current.point).foreach { case (neighbor, moveCost) =>
        val newCost = current.cost + moveCost
        if newCost < bestCosts.getOrElse(neighbor, Long.MaxValue) then
          bestCosts(neighbor) = newCost
          val estimatedTotalCost = newCost + heuristic(neighbor)
          priorityQueue.enqueue(Node(neighbor, newCost, estimatedTotalCost))
      }
    None
