val s = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

val ss = s.split("\n")

val is = ss.map {
  case s"Card $id: $you | $wins" => id.toInt -> ((you.split(" ").filterNot(_.isEmpty).map(_.toInt).toSet, wins.split(" ").filterNot(_.isEmpty).map(_.toInt).toSet))
}

" 6".trim.toInt

val c1 = is.head

0 << (c1._2._2.intersect(c1._2._1).size-1)
1 << 0

is.map { case (_, (you, wins)) =>
  val is = you intersect wins
  if is.isEmpty then 0 else 1 << is.size-1
}

val cs = is.map { case (_, (you, wins)) =>
  val is = you intersect wins
  is.size
}

val cache = Array.fill(cs.length)(1)
cs.zipWithIndex.foreach((cnt, idx) =>
  if cnt > 0 then (1 to cnt).foreach(i => cache(i + idx) += 1 * cache(idx))
)
cache
cache.sum

var a = Array.fill(5)(1)
a(2) = 5
a.patch(2,a.slice(2,4),2)
a.slice(2,4).map(_ + 1)
a