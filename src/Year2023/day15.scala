package year2023.day15

import puzzle.Puzzle
import parse.{*, given}
import scala.collection.mutable

type I = List[String] - ","

object Day15 extends Puzzle[I](2023, 15, "Lens Library"):

//  type DataType = Seq[String]

  def hash(str: String): Int =
    str.foldLeft(0)((acc, c) => (acc + c) * 17 % 256)

  enum Cmd private(val id: String):
    case Set(override val id: String, fl: Int) extends Cmd(id)
    case Rem(override val id: String) extends Cmd(id)

    def boxID: Int = hash(id)

  object Cmd:
    def apply(str: String): Cmd =
      str.split("=") match
        case Array(id, fp) => Set(id, fp.toInt)
        case _ => Rem(str.init)

  def applyCmd(cmd: Cmd)(using boxes: Array[mutable.SeqMap[String, Int]]): Unit =
    val box = boxes(cmd.boxID)
    (cmd, box.contains(cmd.id)) match
      case (Cmd.Set(id, fl), true) => box(id) = fl
      case (Cmd.Set(id, fl), false) => box.addOne(id -> fl)
      case (Cmd.Rem(id), true) => box.remove(id)
      case (Cmd.Rem(_), false) =>

//  override def prep(input: String): DataType =
//    input.replaceAll("\n","").asWords(",")

  override def part1(initSeq: I): Int =
    initSeq.map(hash).sum

  override def part2(initSeq: I): Int =
    val cmds = initSeq.map(Cmd.apply)
    import scala.collection.mutable
    given boxes: Array[mutable.SeqMap[String, Int]] =
      Array.fill(256)(mutable.SeqMap.empty[String, Int])
    cmds.foreach(applyCmd)
    (for
      (b, ib) <- boxes.zipWithIndex
      ((_,fl), il) <- b.zipWithIndex
    yield (ib + 1) * (il + 1) * fl)
      .sum
    
//Day 15: Parabolic Reflector Dish
//  prep: 33.0ms
//  part 1: 11.6ms - 498538
//  part 2: 70.6ms - 286278