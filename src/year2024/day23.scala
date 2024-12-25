package year2024.day23

import year2024.DayOf2024
import parse.{*, given}

type Edge = (String, String)

given Read[LAN] = LAN(_)
//type Connections = Map[String, Set[String]]
//
//given Read[(String, String)] = Read[(String, String)]("-")
//given Read[List[(String, String)]] = Read("\n")
//given Read[Connections] = summon[Read[List[(String, String)]]].map: pairs =>
//  pairs.flatMap((l, r) => List((l, r), (r, l))).foldLeft(Map.empty[String, Set[String]]){case (map, (k, v)) => map.addMulti(k, v)}

case class LAN(s: String):
  lazy val edges: Set[Edge] = s.split("\n")
    .map{ case s"$from-$to" => from -> to }.toSet
  lazy val adjList: Map[String, Set[String]] =
    (edges ++ edges.map(_.swap)).groupMap(_._1)(_._2)

  def findTriplets: List[Set[String]] =
    (for
      (n, ajcs) <- adjList
      n1 <- ajcs
      n2 <- ajcs - n1
      if adjList(n1).contains(n2)
    yield Set(n, n1, n2))
      .toList
      .distinct

  def maximumClique: Set[String] =
    var best: Set[String] = Set.empty

    def bronKerbosh(r: Set[String], p: Set[String], x: Set[String]): Unit =
      if p.isEmpty && x.isEmpty
      then
        { if r.size > best.size then best = r }
      else
        val u = (p ++ x).maxBy(adjList(_).size)
        var p2 = p
        var x2 = x
        for (v <- p -- adjList(u))
          bronKerbosh(r + v, p2 intersect adjList(v), x2 intersect adjList(v))
          p2 -= v
          x2 += v

    bronKerbosh(Set.empty, adjList.keySet, Set.empty)
    best

object Day23 extends DayOf2024[LAN](23, "LAN Party"):

  override def part1(i: LAN): Int =
    i.findTriplets.count(_.exists(_.startsWith("t")))

  override def part2(i: LAN): String =
    i.maximumClique.toSeq.sorted.mkString(",")

//Day 23: LAN Party
//  parse : 2.21ms
//  part 1: 225.ms -> 1370
//  part 2: 83.3ms -> am,au,be,cm,fo,ha,hh,im,nt,os,qz,rr,so