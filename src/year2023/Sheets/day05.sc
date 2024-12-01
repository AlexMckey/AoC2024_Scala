import exts.*

import scala.annotation.tailrec
import scala.collection.mutable

val s = "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4"
  .splitByBlankLines

val sds = s.head.split(':').tail.head.asLongs(" ")

case class Segment(from: Long, to: Long):
  private def points: Seq[Long] = Seq(from, to + 1)
  def break(segs: Seq[Segment]): Seq[Segment] =
    (segs.flatMap(_.points) ++ points)
      .distinct
      .filter(p => p >= from && p <= to + 1)
      .sorted
      .sliding(2)
      .map(p => Segment(p.head, p.last - 1))
      .toSeq
  override def toString: String = s"[$from,$to]"

case class MapRecord(destFrom: Long, sourceFrom: Long, len: Long):
  def decodeFunc: PartialFunction[Long, Long] =
    case x if x >= sourceFrom && x <= sourceTo => x - sourceFrom + destFrom
  def sourceTo: Long = sourceFrom + len - 1
  def segmentSource: Segment = Segment(sourceFrom, sourceTo)

case class Part(what: String, segs: Seq[Segment])

case class DecodeMap(from: String, to: String, maps: Seq[MapRecord]):
  def decodeFunc(s: Segment) = maps
    .map(_.decodeFunc)
    .collectFirst{ case f if f.isDefinedAt(s.from) =>
      Segment(f(s.from), f(s.to))}
    .getOrElse(s)
  private val mapSegments = maps.map(_.segmentSource)
  def decodePart(p: Part): Part =
    val newSegs = p.segs
     .flatMap(_.break(mapSegments))
     .map(decodeFunc)
    Part(to,newSegs)



val dms = s.tail.map(ss =>
  val sm = ss.asStrs
  val ds = sm.tail.map(sa =>
    val Seq(fr, to, ln) = sa.asLongs(" ")
    MapRecord(fr,to,ln)
  )
  sm.head match
    case s"$from-to-$to map:" => DecodeMap(from, to, ds)
)

val s1 = dms.head.maps.head.segmentSource
val s2 = dms.head.maps.last.segmentSource

val maps = Seq(s1,s2)

val sg0 = Segment(40,112)
sg0.break(maps)

val sg1 = Segment(49,98)
sg1.break(maps)

val sg2 = Segment(55,72)
sg2.break(maps)

dms.head.decodePart(Part("seed", Seq(sg0)))
dms.head.decodePart(Part("seed", Seq(sg1)))

val ps = sg1.break(Seq(s1,s2)).flatMap(s => Seq(s.from, s.to))

val ff = (p: Long) => dms.head.maps.map(_.decodeFunc).collectFirst{ case f if f.isDefinedAt(p) => f(p)}.getOrElse(p)

ps.map(ff)
  .grouped(2)
  .map(p => Segment(p.head, p.last))
  .toSeq


val sg3 = Segment(sds.head, sds.head + sds.tail.head-1)
sg3.break(Seq(s1,s2))

dms.head.decodePart(Part("seed", Seq(sg3)))