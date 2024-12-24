package year2024.day24_

import year2024.DayOf2024
import parse.{*, given}
import exts.toLongRadix

import scala.annotation.tailrec

enum GateType:
  case AND, OR, XOR

import GateType.*

type Wire = String
type State = Map[Wire, Int]

case class Gate(w1: Wire, g: GateType, w2: Wire, out: Wire):
  def exec(ws: State): State =
    val (i1, i2) = ws(w1) -> ws(w2)
    val res = g match
      case AND => i1 & i2
      case OR => i1 | i2
      case XOR => i1 ^ i2
    ws.updated(out, res)

type Gates = List[Gate]

type Input = (State, Gates)

given Read[(State, Gates)] = Read.product("\n\n")
given Read[GateType] = GateType.valueOf(_)
given Read[Gate] = Read.product("""(.+) (XOR|AND|OR) (.+) -> (.+)""".r)
given Read[Gates] = Read.seq("\n")
given Read[(Wire,Int)] = Read.product("""(.+): (\d)""".r)
given Read[Map[Wire, Int]] = Read.seq[List, (Wire, Int)]("\n").map(_.toMap)

object Day24_ extends DayOf2024[Input](24, "Crossed Wires"):

  @tailrec
  def calcStableState(gates: List[Gate])(wires: State): State =
    if gates.isEmpty
    then wires
    else
      val (toExec, waits) = gates.partition(g => wires.contains(g.w1) && wires.contains(g.w2))
      calcStableState(waits)(toExec.foldLeft(wires)((state, g) => g.exec(state)))

  extension (st: State)
    def outBin: String =
      st.filter(_._1.startsWith("z"))
        .toList
        .sortBy(_._1)(Ordering[String].reverse)
        .map(_._2)
        .mkString
    def outNum: Long =
      st.outBin.toLongRadix(2)

  override def part1(input: Input): Long =
    calcStableState(input._2)(input._1).outNum

  def printCircuitDot(input: Input): Unit = {
    println("digraph circuit {")
    input._1.foreach {
      (w, i) => println(s"  $w;")
    }
    input._2.foreach{g =>
      println(s"  ${g.out} [label=\"${g.out} ${g.g}\"];")
      println(s"  ${g.w1} -> ${g.out};")
      println(s"  ${g.w2} -> ${g.out};")
    }
    println("}")
  }

  override def part2(input: Input): Int = {
    printCircuitDot(input)
    input._2.size
  }

//Day 24: Crossed Wires
//  parse : 68.5ms
//  part 1: 26.8ms -> 38869984335432
//  part 2: 12.0ms -> 222