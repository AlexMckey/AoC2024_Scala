import AoCLib.exts.*
import AoCLib.exts.IterableExts.*

import scala.util.chaining.*

//val s = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"
val s = "2345A 1\nQ2KJJ 13\nQ2Q2Q 19\nT3T3J 17\nT3Q33 11\n2345J 3\nJ345A 2\n32T3K 5\nT55J5 29\nKK677 7\nKTJJT 34\nQQQJA 31\nJJJJJ 37\nJAAAA 43\nAAAAJ 59\nAAAAA 61\n2AAAA 23\n2JJJJ 53\nJJJJ2 41"
  .asStrs

val cs = "23456789TJQKA"

val csJ = "J23456789TQKA"

case class Hand(cards: String, bid: Int):
  def result(cs: String): (String, String) =
    val ds: Seq[(Char, Int)] = cards
      .groupCount(identity).toSeq
      .sorted(Ordering.by((k,cnt) => cs.indexOf(k) -> -cnt))
      .reverse
    val (ks, cnts) = ds.unzip
    ks.mkString("") -> cnts.mkString("")

val hs = s.map{ case s"$cs $bids" => Hand(cs, bids.toInt) }

hs.map(h => h.bid -> h.result(cs))
  .sortBy((ks, cnts) => cnts -> ks)
  .foreach(println)
  //.zipWithIndex.map((b, i) => b._1 * (i + 1))
  //.sum

hs.map(h => h.bid -> h.cards
    .groupCount(identity)
    .toSeq
    .sorted(Ordering.by((k,cnt) => cnt -> cs.indexOf(k)))
    .reverse
    .map(p => s"${p._2}${p._1}")
    .mkString(""))
  .sortBy(_._2).zipWithIndex
  .map((b,i) => b._1 * (i+1))
  .sum

hs.map(h => h.bid -> csJ.substring(1)
  .map(c => h.cards
    .replaceAll("J", c.toString)
      .groupCount(identity)
      .toSeq
      .map((k,cnt) => cnt -> csJ.indexOf(k))
      .sorted
      .reverse
      .map(p => s"${p._1}${csJ(p._2)}")
      .mkString(""))
    .max)
  .sortBy(_._2)
  .zipWithIndex
  .map((b,i) => b._1 * (i+1))
  .sum

hs.map(h => h.bid -> {
    val jc = h.cards.count(_ == 'J')
    if jc == 5
    then "5J"
    else h.cards
      .filterNot(_ == 'J')
      .groupCount(identity)
      .toSeq
      .sorted(Ordering.by((k,cnt) => cnt -> cs.indexOf(k)))
      .reverse
      .pipe(cs => ((cs.head._1 + jc) -> cs.head._2) +: cs.tail)
      .map(p => s"${p._2}${p._1}")
      .mkString("")}
  ).sortBy(_._2).zipWithIndex
  .map((b,i) => b._1 * (i+1))
  .sum

  //.toSeq.sorted(Ordering[(Int,Int)].on((c,cnt) => (cnt, cs.indexOf(c)))).reverse)
