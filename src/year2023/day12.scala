package year2023.day12

import year2023.DayOf2023
import common.Default
import common.Default.default
import memo.{Cache, Memoize}
import exts.*
import parse.{*, given}

import scala.util.chaining.*

type DataType = Seq[(String, Seq[Int])]

given Read[DataType] with
  override def read(input: String): DataType =
    input.asStrs.map(_.split(" ")
      .pipe(r => r.head -> r.last.asInts(",")))
    
object day12 extends DayOf2023[DataType](12, "Hot Springs"):

  given cache: Cache[(Seq[Char], Seq[Int], Int, Boolean), Long] = Cache.empty

  private def countValid(pat: String, groups: Seq[Int]): Long =
    def count(patRem: Seq[Char], groupsRem: Seq[Int], groupCnt: Int = 0, needGap: Boolean = false): Long =
      Memoize(patRem, groupsRem, groupCnt, needGap){
        (patRem.headOption, groupsRem, groupCnt, needGap) match
          case (None,      Nil,      0, _    ) => 1L
          case (Some('?'), gh +: gt, 0, false) => count(patRem.tail, gt, gh - 1, gh == 1) + count(patRem.tail, groupsRem)
          case (Some('?'), Nil,      0, false) |
               (Some('?'), _,        0, true ) |
               (Some('.'), _,        0, _    ) => count(patRem.tail, groupsRem)
          case (Some('#'), gh +: gt, 0, false) => count(patRem.tail, gt, gh - 1, gh == 1)
          case (Some('?'), _,        _, false) |
               (Some('#'), _,        _, false) => count(patRem.tail, groupsRem, groupCnt - 1, groupCnt == 1)
          case _                               => 0L}
    count(pat.toSeq, groups)

  override def part1(gears: DataType): Long =
    gears.map(countValid).sum

  override def part2(gears: DataType): Long =
    gears.map(g => Array.fill(5)(g._1).mkString("?") -> Seq.fill(5)(g._2).flatten)
      .map(countValid).sum

//Day 12: Hot Springs
//  prep: 70.0ms
//  part 1: 226.ms - 6827
//  part 2: 2.12s - 1537505634471