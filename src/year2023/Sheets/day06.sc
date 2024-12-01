import exts.*

val s = "Time:      7  15   30\nDistance:  9  40  200"
s.asStrs.map(ss => ss.substring(ss.indexOf(":")+1).asInts(" ")).transpose
s.asStrs.map{
  case s"$_: $nums" => nums.asInts(" ")
}
val Vector(times: Seq[Int], distances: Seq[Int]) = s.asStrs.map(_.dropWhile(!_.isDigit).asInts(" ")): @unchecked
val pairs = times.zip(distances)

def dist(t: Int, maxT: Int): Int = t * (maxT - t)
dist(0,7)
dist(2,7)
(1 to 7).map(dist(_,7)).count(_ > 10)
def calcWins(maxT: Int, maxD: Int): Int =
  (1 to maxT).count(t => dist(t, maxT) > maxD)

calcWins(15,40)
calcWins(30,200)
(1 to 30).map(dist(_,30))

pairs.map(calcWins.tupled).product

import parse.{*, given}

case class Num(n: Long)

case class Pars(ls: List[Num ~ """(?:(\d+)\s*)"""] - " ")

type I = List[Pars ~ """\S+:\s*(.*)"""] - "\n"

summon[Read[I]].read(s)