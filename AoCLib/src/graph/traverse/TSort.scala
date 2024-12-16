package graph.traverse

object TSort:
  def apply[A](nodes: Iterable[A], graph: Map[A, Iterable[A]]): List[A] =
    def visit(node: A, visited: Set[A], sorted: List[A]): (Set[A], List[A]) =
      if visited.contains(node) then (visited, sorted)
      else
        val dependencies = graph.getOrElse(node, Nil)
        val (newVisited, newSorted) = dependencies.foldLeft((visited + node, sorted)) {
          case ((vis, sort), dep) => visit(dep, vis, sort)
        }
        (newVisited, node :: newSorted)
    nodes
      .foldLeft((Set.empty[A], List.empty[A])) { case ((visited, sorted), node) =>
        visit(node, visited, sorted)
      }
      ._2
      .reverse
