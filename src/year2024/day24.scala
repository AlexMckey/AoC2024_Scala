package year2024.day24

import year2024.DayOf2024
import parse.{*, given}

type Wire = String

enum GateType:
  case AND, OR, XOR

  def apply(x: Int, y: Int): Int = this match
    case AND => x & y
    case OR => x | y
    case XOR => x ^ y

  def apply(s0: String, s1: String): String =
    Vector(s0, s1).sorted.mkString("(", s" $this ", ")")

object GateType:
  def unapply(s: String): Option[GateType] = Some(GateType.valueOf(s))

import GateType.*

case class Gate(w1: Wire, g: GateType, w2: Wire, out: Wire)

type Wires = Map[Wire, Int]
type Gates = Map[Wire, Gate]

extension (s: String)
  def pad(i: Long) = if i < 10 then s"${s}0$i" else s"$s$i"

case class Circuit(inputs: Wires, gates: Gates):
  def swap(out0: String, out1: String): Circuit =
    val (gate0, gate1) = (gates(out0), gates(out1))
    copy(gates = gates + (out0 -> gate1) + (out1 -> gate0))

  extension (s: String)
    def binary: Long =
      keys(s).foldRight(0L):
        case (key, acc) => (acc << 1) + exec(key, Set.empty)

  def z: Long = "z".binary

  def broken: Boolean = "x".binary + "y".binary != z

  def keys(prefix: String): Vector[String] =
    (inputs.keySet ++ gates.keySet).filter(_.startsWith(prefix)).toVector.sorted

  def exec(key: String, loop: Set[String]): Int =
    if loop(key) then -1
    else gates.get(key) match
      case Some(i0, gate, i1, _) => gate(exec(i0, loop + key), exec(i1, loop + key))
      case None => inputs.getOrElse(key, -1)

  def circuit(key: String, loop: Set[String] = Set.empty): String =
    if loop(key) then ""
    else gates.get(key) match
      case Some(i0, gate, i1, _) => gate(circuit(i0, loop + key), circuit(i1, loop + key))
      case None => key

  def inGates(key: String): Set[String] = gates.get(key) match
    case Some(i0, _, i1, _) => Set(key) ++ inGates(i0) ++ inGates(i1)
    case None => Set.empty

  def carryBit(i: Int): String =
    val (x, y) = ("x".pad(i), "y".pad(i))
    if i == 0 then AND(x, y) else OR(AND(XOR(x, y), carryBit(i - 1)), AND(x, y))

  def addRes(i: Int): String =
    val (x, y) = ("x".pad(i), "y".pad(i))
    if i == 0 then XOR(x, y) else XOR(XOR(x, y), carryBit(i - 1))

  def fix(circuit: Circuit): Iterator[(Vector[String], Circuit)] =
    for
      key <- circuit.keys("z").iterator
      idx = key.tail.toInt
      adder = addRes(idx)
      if circuit.circuit(key) != adder
      key0 <- circuit.inGates(key)
      key1 <- circuit.gates.keySet
      newCircuit = circuit.swap(key0, key1)
      if newCircuit.circuit(key) == adder
    yield Vector(key0, key1) -> newCircuit

  def calc: Iterator[Vector[String]] =
    Iterator.unfold(this): circuit =>
      Option.when(circuit.broken)(fix(circuit).next())

type Input = (Wires, Gates)

given Read[Circuit] = Read.product("\n\n")
given Read[GateType] = GateType.valueOf(_)
given Read[Gate] = Read.product("""(.+) (XOR|AND|OR) (.+) -> (.+)""".r)
given Read[(Wire,Gate)] = summon[Read[Gate]].map(g => g.out -> g)
given Read[Gates] = Read.seq[List, (Wire, Gate)]("\n").map(_.toMap)
given Read[(Wire,Int)] = Read.product("""(.+): (\d)""".r)
given Read[Wires] = Read.seq[List, (Wire, Int)]("\n").map(_.toMap)

object Day24 extends DayOf2024[Circuit](24, "Crossed Wires"):

  override def part1(circuit: Circuit): Long =
    circuit.z

  override def part2(circuit: Circuit): String =
    circuit.calc.flatten.toVector.sorted.mkString(",")

//Day 24: Crossed Wires
//  parse : 65.8ms
//  part 1: 50.0ms -> 38869984335432
//  part 2: 4.40s -> drg,gvw,jbp,jgc,qjb,z15,z22,z35