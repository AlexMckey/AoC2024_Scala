package year2023.day09

import year2023.DayOf2023
import parse.{*, given}
import exts.*

import scala.annotation.tailrec

type DataType = List[List[Int]]

given Read[List[Int]] = Read.seq(" ")
given Read[DataType] = Read.seq("\n")

object Day09 extends DayOf2023[DataType](9, "Mirage Maintenance"):

//  override def prep(input: String): DataType =
//    input.asStrs.map(_.asInts(" "))

  def prediction(seq: List[Int]): (Int, Int) =
    @tailrec
    def seqs(cur: List[Int], acc: List[(Int, Int)] = List.empty): List[(Int, Int)] =
      if cur.forall(_ == 0)
      then acc
      else
        val ss = cur.sliding(2).map(a => a.last - a.head).toList
        seqs(ss, (ss.head -> ss.last) +: acc)

    seqs(seq, List(seq.head -> seq.last))
      .reduce((l, r) => (r._1 - l._1) -> (l._2 + r._2))

  override def part1(seqs: DataType): Long =
    seqs.map(prediction).map(_._2).sum

  override def part2(seqs: DataType): Long =
    seqs.map(prediction).map(_._1).sum

//Day 9: Mirage Maintenance
//  prep: 32.4ms
//  part 1: 47.0ms - 1581679977
//  part 2: 22.8ms - 889
