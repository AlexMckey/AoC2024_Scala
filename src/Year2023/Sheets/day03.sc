import exts.*
import coord.{Pos, pos, given}

import scala.util.matching.Regex

val s = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."

val regP: Regex = "([^.\\d]+)".r
val regPN: Regex = "(\\d+)".r

val regPNS: Regex = "([^.\\d]+)|(\\d+)".r

case class Part(p: Pos, s: String):
  def contains(op: Pos): Boolean =
    p.y == op.y && op.x >= p.x && op.x <= p.x + s.length - 1

regP.findFirstMatchIn(s).get.matched

def takeParts(s: Seq[String], reg: Regex): Seq[Part] =
  s.zipWithIndex
  .flatMap {case (ps,y) =>
    reg.findAllMatchIn(ps).map(m => Part(Pos(m.start, y), m.matched)).toList
  }

def takeParts_(s: Seq[String], reg: Regex): Seq[Part] =
  s.zipWithIndex
    .flatMap {case (ps,y) =>
      reg.findAllMatchIn(ps).map(m => Part(Pos(m.start, y), m.matched)).toList }

takeParts_(s.asStrs, regPNS)      
    
def prep(input: String): (Seq[Part], Seq[Part]) =
  val regP : Regex = "([^.\\d]+)".r
  val regPN: Regex = "(\\d+)".r
  def takeParts(s: Seq[String], reg: Regex): Seq[Part] =
    s.zipWithIndex
     .flatMap { case (ps, y) =>
       reg.findAllMatchIn(ps).map(m => Part(Pos(m.start, y), m.matched)).toList
     }

  val pns = takeParts(input.asStrs, regPN)
  val ps  = takeParts(input.asStrs, regP)
  (ps, pns)

val (ps, pns) = prep(s)

val ptsn = pns.filterNot(pn => ps.flatMap(_.p.nearAll).exists(p => pn.contains(p)))
val pnss = pns.map(_.s.toInt).sum
val ptsns = ptsn.map(_.s.toInt).sum
pnss - ptsns

val psg = ps.filter(_.s == "*")
psg.map(pg => pns.filter(pn => pg.p.nearAll.exists(p => pn.contains(p))))
   .filter(_.size == 2)
   .map(_.map(_.s.toInt).product)
   .sum