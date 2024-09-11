import AoCLib.exts.*
import AoCLib.exts.IterableExts.*

import scala.annotation.tailrec

//val s = "RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)\nDDD = (DDD, DDD)\nEEE = (EEE, EEE)\nGGG = (GGG, GGG)\nZZZ = (ZZZ, ZZZ)"
//val s = "LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)"
val s = "LR\n\n11A = (11B, XXX)\n11B = (XXX, 11Z)\n11Z = (11B, XXX)\n22A = (22B, XXX)\n22B = (22C, 22C)\n22C = (22Z, 22Z)\n22Z = (22B, 22B)\nXXX = (XXX, XXX)"

case class MapDir(l: String, r: String)

val Seq(dir, mapss) = s.splitByBlankLines : @unchecked

val ds = dir.cycle
ds.take(6).toList

val ms = mapss.asStrs.map{
  case s"$from = ($left, $right)" => from -> MapDir(left, right)
}.toMap

val start = "AAA"
val end = "ZZZ"

@tailrec
def go(dirs: Iterator[Char], maps: Map[String, MapDir], cur: Seq[String], end: String => Boolean, steps: Int = 0): Int =
  if cur.forall(end)
  then steps
  else
    val d = dirs.next
    if d == 'L'
    then go(dirs, maps, cur.map(m => maps(m).l), end, steps + 1)
    else go(dirs, maps, cur.map(m => maps(m).r), end, steps + 1)

go(ds, ms, ms.keys.filter(_.endsWith("A")).toSeq, _.endsWith("Z"))
