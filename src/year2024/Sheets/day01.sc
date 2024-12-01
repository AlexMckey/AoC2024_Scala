val s = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"
val is = s.split("\n").map(_.split("""\s+""").map(_.toInt))
is.transpose
val ids = is.unzip(arr => arr.head -> arr.last)
def dist(ab: (Int, Int)): Int =
  scala.math.abs(ab._1 - ab._2)
ids._1.sorted.zip(ids._2.sorted).map(dist).sum
val idsCnt = ids._2.groupMapReduce(identity)(_ => 1)(_ + _)
ids._1.map(id => id * idsCnt.getOrElse(id,0)).sum