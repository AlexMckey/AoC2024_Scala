import AoCLib.exts.*

val s = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
  .asWords(",")

def hash(str: String): Int =
  str.foldLeft(0)((acc,c) => (acc + c) * 17 % 256)

type Lens = (String, Int)

enum Cmd private (val id: String):
  case Set(override val id: String, fp: Int) extends Cmd(id)
  case Rem(override val id: String) extends Cmd(id)

  def boxID = hash(id)

object Cmd:
  def apply(str: String): Cmd =
    str.split("=") match
      case Array(id, fp) => Set(id, fp.toInt)
      case _ => Rem(str.take(2))

val cs = s.map(Cmd.apply)

s.map(hash)

cs.head.boxID
cs.map(_.boxID)

import scala.collection.mutable

val bxs = Array.fill(256)(mutable.SeqMap.empty[String, Int])
val cmd0 = cs.head
def applyCmd(cmd: Cmd): Unit =
  val box = bxs(cmd.boxID)
  (cmd, box.contains(cmd.id)) match
    case (Cmd.Set(id, fp), true) => box(id) = fp
    case (Cmd.Set(id, fp), false) => box.addOne(id -> fp)
    case (Cmd.Rem(id), true) => box.remove(id)
    case (Cmd.Rem(_), false) =>

//applyCmd(cmd0)
cs.foreach(applyCmd)
bxs

bxs.zipWithIndex.flatMap((b,ib) =>
  b.zipWithIndex.map((l,il) => (ib + 1) * (il + 1) * l._2)
).sum