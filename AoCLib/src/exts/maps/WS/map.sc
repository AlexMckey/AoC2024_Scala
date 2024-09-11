import exts.maps.*

val m1 = Map('s' -> "a", 'a' -> "x", 'd' -> "z")
val m2 = Map('s' -> "b", 'f' -> "n")

val m3 = Map('s' -> 1, 'f' -> 2)
val m4 = Map('s' -> 4, 'd' -> 3, 'f' -> 2)

m1.join(m2)
m3.join(m4)

m1.mapAt('s')(_ * 3)

m3.plusAt('f', 5)
m4.plusAt('d', 1)

m1.putMerge('a', "w")(_ + _)
m3.putMerge('f', 4)((a,b) => s"$a$b".toInt)

m1.merge(m2)(_ + _)
m3.merge(m4)(_ + _)