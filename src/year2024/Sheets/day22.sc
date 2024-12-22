val n1 = 42
n1 ^ 15
1 << 6
1 << 11
1 << 24
(1 << 25 - 1)
((1 << 25) - 1) & 100000000L

extension (a: Long)
  infix def mix(b: Long): Long = a ^ b
  def prune: Long = a % 16777216L

  def calcNext: Long =
    var n = a
    n = (n mix (n << 6)).prune
    n = (n mix (n >> 5)).prune
    n = (n mix (n << 11)).prune
    n

  def steps(n: Int): Long =
    Iterator.iterate(a)(_.calcNext)
      .drop(n).next()

100000000L.prune
42L mix 15L

val n = 123L

val r = n.calcNext
r.calcNext

123L.steps(10)
val ns = "1\n10\n100\n2024".split("\n").map(_.toLong)
ns.map(_.steps(2000)).sum
val rn = (1 to 2000).scanLeft(123L)((n,_) => n.calcNext)

val rs = rn.map(_ % 10)
rs.sliding(2).map{ case Seq(a,b) => b - a }.toList

def deltas(l: Seq[Int]): Map[(Int, Int, Int, Int),Int] =
  def rec(a: Int, b: Int, c: Int, d: Int, lst: Seq[Int], acc: Map[(Int, Int, Int, Int),Int]): Map[(Int, Int, Int, Int),Int] =
    //print(s"$a:$b:$c:$d -> ")
    if lst.isEmpty then acc
    else
      val newn = lst.head
      val ds = (b - a, c - b, d - c, newn - d)
      //println(s"$ds = $newn")
      if acc.contains(ds) then rec(b,c,d,newn,lst.tail,acc)
      else rec(b,c,d,newn,lst.tail,acc.updated(ds,newn))

  val Seq(a,b,c,d,tail @ _*) = l
  rec(a,b,c,d,tail,Map.empty)

val rm = deltas(rs.map(_.toInt))
rm((-1,-1,0,2))
rm((-2,1,-1,3))
rm.filter(_._2 == 7).keySet

val nw = "1\n2\n3\n2024".split("\n").map(_.toLong)

val res = nw.map(x => deltas((1 to 2000)
  .scanLeft(x)((n,_) => n.calcNext)
  .map(_ % 10)
  .map(_.toInt))
)
val ds = (-2,1,-1,3)

val r2 = deltas((1 to 2000)
  .scanLeft(2L)((n,_) => n.calcNext)
  .map(_ % 10)
  .map(_.toInt))
r2.filter(_._2 == 7).contains(ds)
r2(ds)

res.map(m => m.getOrElse(ds,0)).sum

import exts.maps.merge

val is = res.map(_.groupMap(_._2)(_._1))//.map(_.map((k,i) => k -> i.toSet))

val ss = res.map(_.keySet).reduce(_ union _)
ss.size

res
  .flatten
  .groupMapReduce(_._1)(_._2)(_ + _).values.max

ss.map(s => res.map(m => m.getOrElse(s,0)).sum).max