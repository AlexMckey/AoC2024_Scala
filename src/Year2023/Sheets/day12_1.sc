import AoCLib.exts.*
import AoCLib.exts.Memoized.{Cache, Memoize}

import scala.annotation.tailrec
import scala.util.chaining.*

val rs = "???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1"
  .asStrs
  .map(_.split(" ").pipe(r => r.head -> r.last.asInts(",")))

given cache: Cache[(Seq[Char], Seq[Int], Int, Boolean), Long] = Cache.empty

def permute(rem: Seq[Char], remGroups: Seq[Int], group: Int = 0, needGap: Boolean = false): Long =
  Memoize(rem, remGroups, group, needGap){
    (rem, remGroups, group, needGap) match
      case (Nil,      Nil,      0, _)     => 1L
      case ('?' +: t, gh +: gt, 0, false) => permute(t, gt, gh - 1, gh == 1) + permute(t, remGroups, 0 , false)
      case ('?' +: _, Nil,      0, false)
         | ('?' +: _, _,        0, true)
         | ('.' +: _, _,        0, _)     => permute(rem.tail, remGroups, 0, false)
      case ('#' +: t, gh +: gt, 0, false) => permute(t, gt, gh - 1, gh == 1)
      case ('?' +: _, _,        _, false)
         | ('#' +: _, _,        _, false) => permute(rem.tail, remGroups, group - 1, group == 1)
      case _                              => 0L}

rs.head._2
rs.head._1.toSeq
(rs.head._1.toSeq, rs.head._2, 0, false) match
  case ('?' +: t, gh +: gt, 0, false) => println("1")
  case ('?' +: t, _, g, false)        => println("4")
  case _ => println("Q")

permute(rs.head._1.toSeq, rs.head._2)
permute(rs.tail.head._1.toSeq, rs.tail.head._2)

rs.map(permute(_,_))

val brs = rs.map((p,a) => Array.fill(5)(p).mkString("?") -> Seq.fill(5)(a).flatten)
brs.head.pipe(permute(_,_))
brs.tail.head.pipe(permute(_,_))