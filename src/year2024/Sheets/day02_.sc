import scala.annotation.tailrec

val s = "16 10 13 15 17\n13 18 15 16 17\n10 13 15 19\n17 18 13 15 16\n17 10 13 15 16\n10 13 15 18 9\n10 13 15 18 11"
//val s = "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"
val ls = s.split("\n").map(_.split(" ").map(_.toInt).toList).toList

val sl = ls.map(_.sliding(2).map{ case List(a,b) => a - b }.toList)

def check(l: List[Int], cntBadSkipped: Int = 0): Boolean =
  val sl = l.sliding(2).map{ case List(a,b) => a - b }.toList
  def rec(prev: Int, l: List[Int], skipped: Int = 0, res: Boolean = true): Boolean =
    println(s"$prev :: $l = $res , $skipped")
    if !res || skipped < 0 then false
    else if l.isEmpty && skipped >= 0 then true
    else if l.isEmpty then (1 to 3).contains(prev.abs)
    else
      val v = l.head
      val sb = prev.sign == v.sign
      val db = (1 to 3).contains(prev.abs)
      if sb && db then rec(v, l.tail, skipped)
      else rec(v, l.tail, skipped - 1) || rec(prev + v, l.tail, skipped - 1)
  rec(sl.head, sl.tail, cntBadSkipped)
  
check(ls(0))
check(ls(0),1)

check(ls(1))
check(ls(1),1)

check(ls(2))
check(ls(2),1)

check(ls(3))
check(ls(3),1)

check(ls(4))
check(ls(4),1)

ls(5)
check(ls(5))
check(ls(5),1)

check(ls(6))
check(ls(6),1)
