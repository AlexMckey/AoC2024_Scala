val s = "#####\n.####\n.####\n.####\n.#.#.\n.#...\n.....\n\n#####\n##.##\n.#.##\n...##\n...#.\n...#.\n.....\n\n.....\n#....\n#....\n#...#\n#.#.#\n#.###\n#####\n\n.....\n.....\n#.#..\n###..\n###.#\n###.#\n#####\n\n.....\n.....\n.....\n#....\n#.#..\n#.#.#\n#####"

val ss = s.split("\n\n")

enum Kind {
  case Lock, Key
}

import Kind.*

case class KeyLock(kind: Kind, pins: Array[Int]) {
  lazy val size: Int = pins.max
  def isFit(other: KeyLock): Boolean =
    if other.kind == kind then false
    else
      val ad = pins.zip(other.pins).map(_ + _)
      println(ad.toList)
      ad.forall(_ <= 5)
}


val (ls, ks) = ss.map(_.split("\n")).map{ a =>
  val cnts = a.map(_.toCharArray).transpose.map(_.count(_ == '#')-1)
  if a.head == "#####"
  then KeyLock(Lock, cnts)
  else KeyLock(Key, cnts)
}.partition(_.kind == Lock)

ls.toList
ks.toList

val fs =
  for
    l <- ls
    k <- ks
    if l.isFit(k)
  yield l -> k
fs.size