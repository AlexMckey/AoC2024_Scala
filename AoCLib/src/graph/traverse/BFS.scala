package graph.traverse

import scala.annotation.tailrec
import scala.collection.mutable

object BFS:
  def traverse[A](start: A)(ns: A => Iterable[A]): Map[A, Int] =
    @tailrec
    def rec(visited: Map[A, Int], toVisit: Map[A, Int]): Map[A, Int] =
      val neighbors = for {
        (node, cost) <- toVisit; newNode <- ns(node)
      } yield newNode -> (cost + 1)
      val newVisited = visited ++ toVisit
      val newToVisit = neighbors.filterNot(n => visited.contains(n._1))
      if newToVisit.isEmpty then newVisited else rec(newVisited, newToVisit)

    rec(Map.empty, Map(start -> 0))
  end traverse

//  def traverseMut[A](start: A)(neighbors: A => Iterable[A]): Map[A, Int] =
//    val visitedDistance: mutable.Map[A, Int] = mutable.Map.empty
//    val toVisit: mutable.Queue[(Int, A)] = mutable.Queue.empty
//
//    def enqueue(node: A, dist: Int): Unit = toVisit.enqueue(dist -> node)
//
//    enqueue(start, 0)
//
//    while toVisit.nonEmpty do
//      val (dist, node) = toVisit.dequeue()
//      if !visitedDistance.contains(node)
//      then
//        visitedDistance(node) = dist
//
//        def goNeighbor(newNode: A): Unit =
//          if !visitedDistance.contains(newNode)
//          then enqueue(newNode, dist + 1)
//
//        neighbors(node).iterator.foreach(goNeighbor)
//    end while
//
//    visitedDistance.toMap
//  end traverseMut

  def stepCount[A](start: A, goal: A => Boolean)(ns: A => Iterable[A]): Option[Int] =
    @tailrec
    def rec(toVisit: Iterable[A], cost: Map[A, Int]): Option[Int] = toVisit match
      case h :: _ if goal(h) => Some(cost(h))
      case h :: t =>
        val neighbors = ns(h).filterNot(cost.contains)
        rec(t ++ neighbors, cost ++ neighbors.map(n => n -> (cost(h) + 1)))
      case _ => None

    rec(List(start), Map(start -> 0))
  end stepCount

  def search[A](start: A, goal: (A, Int) => Boolean)(ns: A => Iterable[A]): (Option[(A, Int)], Map[A, Int]) =
    val visitedDistance: mutable.Map[A, Int] = mutable.Map.empty
    val toVisit: mutable.Queue[(Int, A)] = mutable.Queue.empty

    def enqueue(node: A, dist: Int): Unit = toVisit.enqueue(dist -> node)

    enqueue(start, 0)

    while toVisit.nonEmpty do
      val (dist, node) = toVisit.dequeue()
      if (!visitedDistance.contains(node))
        visitedDistance(node) = dist

        if goal(node, dist)
        then return Some(node -> dist) -> visitedDistance.toMap

        def goNeighbor(newNode: A): Unit =
          if !visitedDistance.contains(newNode)
          then enqueue(newNode, dist + 1)

        ns(node).iterator.foreach(goNeighbor)
    end while

    None -> visitedDistance.toMap
  end search

  def search[A](start: A, goal: A)(ns: A => Iterable[A]): (Option[(A, Int)], Map[A, Int]) =
    search(start, _ == goal)(ns)

  def search[A](start: A, goal: A => Boolean)(ns: A => Iterable[A]): (Option[(A, Int)], Map[A, Int]) =
    search(start, (t, _) => goal(t))(ns)

  def components[A](starts: Set[A])(ns: A => Iterable[A]): Set[Set[A]] =
    def bfsGroups(nodes: Set[A]): Set[Set[A]] =
      if (nodes.isEmpty) Set.empty
      else
        val startNode = nodes.head
        val group = traverse(startNode)(ns).keySet
        val restNodes = nodes -- group
        bfsGroups(restNodes) + group

    bfsGroups(starts)
  end components
