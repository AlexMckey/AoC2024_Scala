package graph.traverse.other

import struct.PriorityQueue

import scala.annotation.tailrec
import scala.math.Numeric.Implicits.infixNumericOps
import scala.math.Ordered.orderingToOrdered
//import scala.math.Ordering.Implicits.infixOrderingOps

class AStar[Position, Weight : Numeric](
                                         goal: Position => Boolean,
                                         // The heuristic must be <= the actual cost, but is more efficient when
                                         // closer to the actual cost. _ => 0 is equivalent to Dijkstra's algorithm.
                                         heuristic: Position => Weight,
                                         neighborWeight: (Position, Position) => Weight, // current -> neighbor -> weight
                                         initial: Weight, // initial weight of the start state, usually zero
                                         getNeighbors: Position => Set[Position]):

  def getPath(start: Position)(using CanEqual[Position, Position]) : List[Position] =
    @tailrec
    def helper(open: PriorityQueue[Position, Weight], cameFrom: Map[Position, Position],
               g: Map[Position, Weight]): List[Position] =

      if open.isEmpty then
        return List.empty[Position]

      val (current, openWithoutMin) = open.dequeue.get
      if goal(current) then
        return reconstructPath(current, cameFrom)

      def tentativeG(neighbor: Position) = g(current) + neighborWeight(current, neighbor)

      val neighbors = getNeighbors(current) - current
      val betterNeighbors = neighbors filter {neighbor => g.get(neighbor).map(tentativeG(neighbor) < _).getOrElse(true)}

      val newCameFrom = cameFrom ++ betterNeighbors.map{(_, current)}
      val newG = g ++ betterNeighbors.map{neighbor => (neighbor, tentativeG(neighbor))}
      val updatedH = betterNeighbors.map{neighbor => (neighbor, tentativeG(neighbor) + heuristic(neighbor))}

      val newOpen = openWithoutMin.enqueue(updatedH)

      helper(newOpen, newCameFrom, newG)

    val open = PriorityQueue(start -> heuristic(start))
    val cameFrom = Map[Position, Position]()
    val g = Map[Position, Weight](start -> initial)
    helper(open, cameFrom, g)

  def getMinCost(starts: Position*): Option[Weight] =
    @tailrec
    def helper(open: PriorityQueue[Position, Weight], g: Map[Position, Weight]): Option[Weight] =

      if open.isEmpty then
        return None

      val (current, openWithoutMin) = open.dequeue.get
      if goal(current) then
        return Some(g(current))

      def tentativeG(neighbor: Position) = g(current) + neighborWeight(current, neighbor)

      val neighbors = getNeighbors(current) - current
      val betterNeighbors = neighbors filter {neighbor => g.get(neighbor).map(tentativeG(neighbor) < _).getOrElse(true)}

      val newG = g ++ betterNeighbors.map{neighbor => (neighbor, tentativeG(neighbor))}
      val updatedH = betterNeighbors.map{neighbor => (neighbor, tentativeG(neighbor) + heuristic(neighbor))}

      val newOpen = openWithoutMin.enqueue(updatedH)

      helper(newOpen, newG)

    val open = starts.map(start => (start -> heuristic(start))).foldLeft(PriorityQueue.Empty: PriorityQueue[Position, Weight])((queue, value) => queue.enqueue(value))
    val g = starts.map(start => start -> initial).toMap
    helper(open, g)

  enum Step:
    case Visited(position: Position)
    case Opened(positions: Set[Position])
    case FoundPath(path: List[Position])
    case Failed
    
  import Step.*
    
//  sealed trait Step
//  case class Visited(position: Position) extends Step
//  case class Opened(positions: Set[Position]) extends Step
//  case class FoundPath(path: List[Position]) extends Step
//  case object Failed extends Step

  def visualize(starts: Position*): LazyList[Step] =
    def helper(open: PriorityQueue[Position, Weight], opened: Set[Position], cameFrom: Map[Position, Position],
               g: Map[Position, Weight]): LazyList[Step] =

      if open.isEmpty then
        return LazyList(Failed)

      val (current, openWithoutMin) = open.dequeue.get
      if goal(current) then
        return LazyList(FoundPath(reconstructPath(current, cameFrom)))

      def tentativeG(neighbor: Position) = g(current) + neighborWeight(current, neighbor)

      val neighbors = getNeighbors(current) - current
      val betterNeighbors = neighbors filter {neighbor => g.get(neighbor).forall(tentativeG(neighbor) < _) }

      val newCameFrom = cameFrom ++ betterNeighbors.map{(_, current)}
      val newG = g ++ betterNeighbors.map{neighbor => (neighbor, tentativeG(neighbor))}
      val updatedH = betterNeighbors.map{neighbor => (neighbor, tentativeG(neighbor) + heuristic(neighbor))}

      val newOpen = openWithoutMin.enqueue(updatedH)
      val newOpened = betterNeighbors -- opened
      val newSteps = if newOpened.isEmpty then LazyList(Visited(current)) else LazyList(Visited(current), Opened(newOpened))

      newSteps ++ helper(newOpen, opened ++ newOpened, newCameFrom, newG)

    val open = starts.map(start => start -> heuristic(start)).foldLeft(PriorityQueue.Empty: PriorityQueue[Position, Weight])((queue, value) => queue.enqueue(value))
    val g = starts.map(start => start -> initial).toMap
    val cameFrom = Map[Position, Position]()
    helper(open, Set.empty, cameFrom, g)

  private def reconstructPath(end: Position, cameFrom: Map[Position, Position]): List[Position] =
    @tailrec
    def helper(current: Position, result: List[Position]): List[Position] =
      cameFrom.get(current) match
        case None => result
        case Some(previous) => helper(previous, previous :: result)

    helper(end, List(end))
