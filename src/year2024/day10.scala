package year2024.day10

import year2024.DayOf2024
import parse.{*, given}
import coord.{Pos, given}
import grid.MapGrid
import grid.CharGrid

import scala.collection.mutable

type I = Seq[Map[Pos, Int]]

given Read[Seq[Map[Pos,Int]]] with
  override def read(input: String): Seq[Map[Pos, Int]] =
    def neighbors(g: CharGrid)(p: Pos): Iterator[Pos] =
      p.nearAxis.filter(g.contains).filter(n => g(n) - g(p) == 1)

    def traverse[A](start: A, neighbors: A => IterableOnce[A]): Map[A, Int] =
      val visitedCnt: mutable.Map[A, Int] = mutable.Map.empty.withDefaultValue(0)
      val toVisit: mutable.Queue[A] = mutable.Queue.empty
      toVisit.enqueue(start)
      while toVisit.nonEmpty do
        val node = toVisit.dequeue()
        visitedCnt(node) += 1
        toVisit.appendAll(neighbors(node))
      end while
      visitedCnt.toMap
    end traverse

    val g = MapGrid(input)
    val starts = g.filter(_ == '0').allPos.toList
    val ends = g.filter(_ == '9').allPos
    starts.map(s => traverse(s, neighbors(g))
      .view
      .filterKeys(ends.contains)
      .toMap)

object Day10 extends DayOf2024[I](10, "Hoof It"):

  override def part1(sm: I): Long =
    sm.map(_.keySet.size).sum

  override def part2(sm: I): Long =
    sm.map(_.values.sum).sum

//Day 10: Hoof It
//  parse : 315.ms
//  part 1: 1.67ms -> 674
//  part 2: 4.33ms -> 1372