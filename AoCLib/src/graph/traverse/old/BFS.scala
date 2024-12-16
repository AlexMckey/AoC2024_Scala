package graph.traverse.old

import scala.annotation.tailrec
import scala.collection.mutable

object BFS:

  def traverseCond[A](
    start: A,
    neighbors: A => IterableOnce[(A, Int)],
    stopCond: Map[A, Int] => Boolean
  ): Map[A, Int] =
    val visitedDistance: mutable.Map[A, Int]     = mutable.Map.empty
    val toVisit: mutable.PriorityQueue[(Int, A)] = mutable.PriorityQueue.empty(Ordering.by(-_._1))

    def enqueue(node: A, dist: Int): Unit = toVisit.enqueue(dist -> node)

    enqueue(start, 0)

    while toVisit.nonEmpty || !stopCond(visitedDistance.toMap) do
      val (dist, node) = toVisit.dequeue()
      if !visitedDistance.contains(node)
      then
        visitedDistance(node) = dist

        def goNeighbor(newNode: A, distDelta: Int): Unit =
          if !visitedDistance.contains(newNode)
          then enqueue(newNode, dist + distDelta)

        neighbors(node).iterator.foreach(goNeighbor.tupled)
    end while

    visitedDistance.toMap
  end traverseCond

  def traverse[A](start: A, neighbors: A => IterableOnce[A]): Map[A, Int] =
    val visitedDistance: mutable.Map[A, Int] = mutable.Map.empty
    val toVisit: mutable.Queue[(Int, A)]     = mutable.Queue.empty

    def enqueue(node: A, dist: Int): Unit = toVisit.enqueue(dist -> node)

    enqueue(start, 0)

    while toVisit.nonEmpty do
      val (dist, node) = toVisit.dequeue()
      if !visitedDistance.contains(node)
      then
        visitedDistance(node) = dist

        def goNeighbor(newNode: A): Unit =
          if !visitedDistance.contains(newNode)
          then enqueue(newNode, dist + 1)

        neighbors(node).iterator.foreach(goNeighbor)
    end while

    visitedDistance.toMap
  end traverse

  def search[A](start: A, target: A, neighbors: A => IterableOnce[A]): (Option[(A, Int)], Map[A, Int]) =
    search(start, _ == target, neighbors)

  def search[A](
    start: A,
    isTarget: A => Boolean,
    neighbors: A => IterableOnce[A]
  ): (Option[(A, Int)], Map[A, Int]) =
    search(start, (t, _) => isTarget(t), neighbors)

  def search[A](
    start: A,
    isTarget: (A, Int) => Boolean,
    neighbors: A => IterableOnce[A]
  ): (Option[(A, Int)], Map[A, Int]) =
    val visitedDistance: mutable.Map[A, Int] = mutable.Map.empty
    val toVisit: mutable.Queue[(Int, A)]     = mutable.Queue.empty

    def enqueue(node: A, dist: Int): Unit = toVisit.enqueue(dist -> node)

    enqueue(start, 0)

    while toVisit.nonEmpty do
      val (dist, node) = toVisit.dequeue()
      if (!visitedDistance.contains(node))
        visitedDistance(node) = dist

        if isTarget(node, dist)
        then return Some(node -> dist) -> visitedDistance.toMap

        def goNeighbor(newNode: A): Unit =
          if !visitedDistance.contains(newNode)
          then enqueue(newNode, dist + 1)

        neighbors(node).iterator.foreach(goNeighbor)
    end while

    None -> visitedDistance.toMap
  end search

  def components[A](nodes: Set[A], neighbors: A => IterableOnce[A]): Set[Set[A]] =

    def bfsGroups(nodes: Set[A]): Set[Set[A]] =
      if (nodes.isEmpty) Set.empty
      else
        val startNode = nodes.head
        val group     = traverse(startNode, neighbors).keySet
        val restNodes = nodes -- group
        bfsGroups(restNodes) + group

    bfsGroups(nodes)
  end components
