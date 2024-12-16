package year2024.day16

import year2024.DayOf2024
import parse.{*, given}
import grid.{CharGrid, VectorGrid}
import VectorGrid.charVectorGridReader
import coord.{Dir, Pos}
import graph.traverse.old.Dijkstra
import walker.Walker

type Maze = CharGrid

object Day16 extends DayOf2024[Maze](16, "Reindeer Maze"):

  def ns(go: Walker => Walker, m: CharGrid)(w: Walker): Seq[(Walker, Int)] =
    val newW = go(w)
    Seq(w.turnLeft -> 1000, w.turnRight -> 1000) ++
      (if m(newW.p) != '#' then Seq(newW -> 1) else Seq.empty)

  def backPaths(m: CharGrid, wi: Map[Walker, Int], finish: Walker, w: Walker): Set[Pos] =
    if w == finish
    then Set(w.p)
    else
      val ds = wi(w)
      val ps =
        for
          (oldSt, step) <- ns(_.prev, m)(w)
          oldD <- wi.get(oldSt)
          if oldD + step == ds
        yield backPaths(m, wi, finish, oldSt) + w.p
      ps.foldLeft(Set.empty)(_ ++ _)

  override def part1(m: Maze): Long =
    val start = Walker(m.find('S').get, Dir.E)
    val end = m.find('E').get
    Dijkstra.search[Walker](start, _.p == end, ns(_.step, m)).get._2

  override def part2(m: Maze): Long =
    val start: Walker = Walker(m.find('S').get, Dir.E)
    val path: Map[Walker, Int] = Dijkstra.traverse[Walker](start, ns(_.step, m))
    val end: Pos = m.find('E').get
    val goal = path.filter(_._1.p == end).minBy(_._2)._1
    backPaths(m, path, start, goal).size

//Day 16: Reindeer Maze
//  parse : 14.0ms
//  part 1: 176.ms -> 90440
//  part 2: 248.ms -> 479