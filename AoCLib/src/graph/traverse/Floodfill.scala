package graph.traverse

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Floodfill:
  def apply[A](start: A, nf: A => Iterable[A])(ff: A => Boolean): Set[A] =
    if !ff(start) then Set.empty
    else
      @tailrec
      def helper(visited: Set[A], open: Queue[A]): Set[A] =
        open.dequeueOption match
          case Some((current, open)) =>
            val neighbors  = nf(current).filter(ff).toSet -- visited
            val newVisited = visited ++ neighbors
            val newOpen    = open.enqueueAll(neighbors)
            helper(newVisited, newOpen)
          case None => visited
      helper(Set(start), Queue(start))
