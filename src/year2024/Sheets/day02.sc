import scala.annotation.tailrec

val s = "16 10 13 15 17\n13 18 15 16 17\n10 13 15 19\n17 18 13 15 16\n17 10 13 15 16\n10 13 15 18 9\n10 13 15 18 11"
//val s = "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"
val ls = s.split("\n").map(_.split(" ").map(_.toInt).toList).toList
val sl = ls(1).sliding(2).map{ case List(a,b) => a - b }.toList
(sl.forall(_ > 0) || sl.forall(_ < 0)) && sl.forall(_.abs <= 3)

def checkSafeBadLevel(l: List[Int], skipOneBad: Boolean = false): Boolean =
  val goalSign = (l.head - l.last).sign
  val sl = l.sliding(2).map { case List(a, b) => a - b }.toList
  val bl = sl.forall(d => (d.sign == goalSign) && (1 to 3).contains(d.abs))
  //println(s"$sl : $bl")
  if !bl && skipOneBad then
    l.indices.exists{ i =>
      val (ll, lr) = l.splitAt(i)
      checkSafeBadLevel(ll ++ lr.tail)
    }
  else bl

def check(l: List[Int], skipOneBad: Boolean = false): Boolean =
  val goalSign = (l.head - l.last).sign
  @tailrec
  def rec(prev: Int, l: List[Int], res: Boolean = true, skipped: Int = 0): Boolean =
    println(s"$prev :: $l = $res , $skipped")
    if !res || skipped > 1 then false
    else if l.isEmpty then res
    else
      val v = l.head
      val sb = (prev - v).sign == goalSign
      val db = (prev - v).abs >= 1 && (prev - v).abs <= 3
      if sb && db then rec(v, l.tail, true, skipped)
      else if skipOneBad then
        rec(prev, l.tail, true, skipped + 1)
          //|| rec(v, l.tail, true, skipped + 1)
      else rec(v, l.tail, false, skipped)
  rec(l.head, l.tail) ||
    rec(l.tail.head, l.tail.tail, true, 1) ||
    rec(l.head, l.tail.init, true, 1)

checkSafeBadLevel(ls(0))
ls.map(checkSafeBadLevel(_))
val newls = ls(0).indices.map{ i =>
  val (ll, lr) = ls(0).splitAt(i)
  ll ++ lr.tail
}

val (ll, lr) = ls(1).splitAt(3)
val newL = ll ++ lr.tail
checkSafeBadLevel(newL)
ls.map(checkSafeBadLevel(_, true))

