import scala.collection.mutable

val s = "r, wr, b, g, bwu, rb, gb, br\n\nbrwrr\nbggr\ngbbr\nrrbgbr\nubwu\nbwurrg\nbrgr\nbbrgwb"
val Array(ts,ds) = s.split("\n\n")
val towels: Array[String] = ts.split(", ")
val designs: Array[String] = ds.split("\n")

val d = designs.last
towels.collect{ case t if d.startsWith(t) => List(t) -> d0.substring(t.length)}

def ns(d: String): Seq[String] =
  towels.collect{ case t if d.startsWith(t) => d.substring(t.length)}

ns(d)

import graph.traverse.BFS
BFS.stepCount[String](d, _.isBlank)(ns)
BFS.traverse[String](designs.last)(ns)

def collectDesign(d: String, towels: List[String]): List[List[String]] = {
  def cut(cur: String, acc: List[String] = List.empty): List[(String, List[String])] =
    towels.collect { case t if cur.startsWith(t) => cur.substring(t.length) -> (t +: acc) }

  def rec(toCut: List[(String, List[String])], acc: List[List[String]] = List.empty): List[List[String]] = {
    if toCut.isEmpty then acc
    else {
      val d = toCut.head
      if d._1.isBlank then
        rec(toCut.tail, toCut.head._2 +: acc)
      else {
        val newCuts = cut.tupled(d)
        if newCuts.isEmpty
        then rec(toCut.tail, List.empty +: acc)
        else rec(newCuts ++ toCut.tail, acc)
      }
    }
  }

  rec(List(d -> List.empty))
}
val d0 = designs(0)
val d4 = designs(4)
collectDesign(d0, towels.toList)
towels.toList.collect { case t if d0.startsWith(t) => d0.substring(t.length) -> t }
collectDesign(d4, towels.toList)
towels.toList.collect { case t if d4.startsWith(t) => d4.substring(t.length) -> t }

val res = designs.map(d => collectDesign(d, towels.toList))
res.count(_.exists(_.nonEmpty))
res.map(_.count(_.nonEmpty)).sum
def countVariation(d: String, towel: List[String]): Int = {
  val dp = mutable.Map.empty[Int, Int]

  def rec(idx: Int): Int = {
    dp.getOrElseUpdate(idx, {
      if idx == d.length then 1
      else towel.filter(t => d.startsWith(t, idx)).map(t => rec(idx + t.length)).sum
    })
  }

  rec(0)
}
countVariation(d4, towels.toList)
designs.map(countVariation(_, towels.toList))

val tRE = ("^(" + towels.mkString("|") + ")*$").r
tRE.matches(d4)