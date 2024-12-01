import AoCLib.exts.*

import scala.annotation.tailrec
import scala.util.chaining.*

case class Rows(pat: String, gs: Seq[Int]):
  private def isValid(s: String): Boolean =
    s.split("\\.+").filterNot(_.isEmpty).map(_.length).sameElements(gs)
  def validCount: Int =
    @tailrec
    def rec(acc: Seq[String], i: Int = 0): Seq[String] =
      if pat.length == i
      then acc
      else
        rec(acc.flatMap(s => if s(i) == '?' then Seq(s.patch(i, ".", 1), s.patch(i, "#", 1)) else Seq(s)), i + 1)
    rec(Seq(pat)).count(isValid)

val rs = "???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1"
  .asStrs
  .map(_.split(" ").pipe(r => Rows(r.head, r.last.asInts(","))))

//val svp = "#.#.###"
val svp = "####.#...#..."

svp.split("\\.+").map(_.length).sameElements(rs(3).gs)
rs(3).pat.split("\\.+").map(s => s -> s.length)
rs(2).pat.split("\\.+").map(s => s -> s.length)

Seq(rs.head.pat).map(s => if s(0) == '?' then Seq(s.patch(0,".",1),s.patch(0,"#",1)) else Seq(s))

@tailrec
def vars(acc: Seq[String] = Seq.empty, i: Int = 0): Seq[String] =
  if acc.head.length == i then acc
  else
    vars(acc.flatMap(s => if s(i) == '?' then Seq(s.patch(i,".",1),s.patch(i,"#",1)) else Seq(s)), i + 1)

val r1 = vars(Seq(rs.head.pat))
val r2 = vars(Seq(rs.tail.head.pat))

def isValid(s: String, gs: Seq[Int]): Boolean =
  s.split("\\.+").filterNot(_.isEmpty).map(_.length).sameElements(gs)

r1.count(s => isValid(s,rs.head.gs))
rs.tail.head.gs
r2.map(s => s.split("\\.+").filterNot(_.isEmpty).map(_.length))
r2.count(s => isValid(s,rs.tail.head.gs))

rs.map(_.validCount).sum