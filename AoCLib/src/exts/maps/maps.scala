package exts.maps

type A = Any

//def merge[A, B](a: Map[A, B], b: Map[A, B])(mergef: (B, Option[B]) => B): Map[A, B] =
//  val (big, small) = if a.size > b.size then a -> b else b -> a
//  small.foldLeft(big) { case (z, (k, v)) => z + (k -> mergef(v, z.get(k))) }
//
//def mergeIntSum[A](a: Map[A, Int], b: Map[A, Int]): Map[A, Int] =
//  merge(a, b)((v1, v2) => v2.map(_ + v1).getOrElse(v1))

extension [K, B <: A, C <: A](left: Map[K, B])

  def putMerge(key: K, value: B)(merge: (B, B) => B): Map[K, B] =
    left.updated(key, left.get(key).fold(value)(merge(_, value)))

  def mapAt(key: K)(f: B => B): Map[K, B] = left.updated(key, f(left(key)))

  def merge(that: Iterable[(K, B)])(m: (B, B) => B): Map[K, B] =
    that.foldLeft(left) {
      case (acc, (key, value)) => acc.putMerge(key, value)(m)
    }
    
  def join(right: Map[K, C]): Map[K, Seq[A]] =
    val inter = left.keySet.intersect(right.keySet)

    val leftFiltered = left.view.filterKeys(inter.contains)
    val rightFiltered = right.view.filterKeys(inter.contains)

    (leftFiltered.toSeq ++ rightFiltered.toSeq)
      .groupMapReduce(_._1)((_,v) => Seq(v))(_ ++ _)
//      .view
//      .mapValues(_.map {
//        _._2
//      }.toList)
//      .toMap

extension [K, V](map: Map[K, V])(using V: Numeric[V])
  def plusAt(key: K, delta: V = V.one): Map[K, V] =
    map + (key -> V.plus(map.getOrElse(key, V.zero), delta))