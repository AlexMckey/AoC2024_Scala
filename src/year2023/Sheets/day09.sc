import AoCLib.exts.IterableExts.*
import AoCLib.exts.*

import scala.annotation.tailrec

val s = "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45"

val sqs = s.asStrs.map(_.asInts(" "))

val sq1 = sqs.head
val sq1_1 = sq1.sliding(2).map(a => a.last - a.head).toList
val sq1_2 = sq1_1.sliding(2).map(a => a.last - a.head).toList

def lastPrediction(seq: Seq[Int]): Int =
  @tailrec
  def seqs(cur: Seq[Int], acc: Seq[Int] = Seq.empty): Int =
    if cur.forall(_ == 0)
    then acc.sum
    else
      val ss = cur.sliding(2).map(a => a.last - a.head).toSeq
      seqs(ss, ss.last +: acc)
  seq.last + seqs(seq)

def firstPrediction(seq: Seq[Int]): Int =
  @tailrec
  def seqs(cur: Seq[Int], acc: Seq[Int] = Seq.empty): Int =
    if cur.forall(_ == 0)
    then acc.sum
    else
      val ss = cur.sliding(2).map(a => a.last - a.head).toSeq
      seqs(ss, ss.head +: acc)
  seq.head - seqs(seq)

lastPrediction(sq1)
lastPrediction(sqs.tail.head)
lastPrediction(sqs(2))

firstPrediction(sq1)
firstPrediction(sqs.tail.head)
firstPrediction(sqs(2))

@tailrec
def seqs(cur: Seq[Int], acc: Seq[(Int,Int)] = Seq.empty): Seq[(Int,Int)] =
  if cur.forall(_ == 0)
  then acc
  else
    val ss = cur.sliding(2).map(a => a.last - a.head).toSeq
    seqs(ss, (ss.head -> ss.last) +: acc)

val s1 = "10  13  16  21  30  45".asInts(" ")
val r1 = seqs(s1, Seq(s1.head -> s1.last))

def difference(history: Seq[Int]): Seq[Int] = (history.tail lazyZip history).map(_ - _)
def differences(history: Seq[Int]): Seq[Int] = (history lazyZip history.tail).map(_ - _)

difference(s1)
differences(s1)

r1.reduceRight((l,r) => (l._1 - r._1) -> (l._2 + r._2))

val res1 = LazyList.iterate(s1)(differences)
        .takeWhile(!_.forall(_ == 0))
        .map(_.head)
        .sum

firstPrediction(s1)
