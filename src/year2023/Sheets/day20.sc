import AoCLib.exts.*

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.collection.mutable.Map as MMap

type DataType = Map[String, Module]

type Signal = (String, Boolean, String)

extension (s: Signal)
  def from: String    = s._1
  def dest: String    = s._3
  def signal: Boolean = s._2

val EmptySignal = List.empty[Signal]

sealed trait Module:
  def out: List[String]
  def name: String
  def logic(in: Signal): List[Signal] = EmptySignal
  def sendSignal(signal: Boolean): List[Signal] =
    out.map(o => (name, signal, o))

case class Flipflop(name: String, out: List[String]) extends Module:
  private var isOn: Boolean = false
  override def logic(in: Signal): List[Signal] =
    (isOn, in.signal) match
      case (_, true) => EmptySignal
      case (_, false) =>
        isOn = !isOn
        sendSignal(isOn)

case class Conjunction(name: String, out: List[String]) extends Module:
  private val ins: MMap[String, Boolean] = MMap.empty
  def addIns(is: List[String]): Unit =
    ins.addAll(is.map(_ -> false))
  override def logic(in: Signal): List[Signal] =
    ins.update(in.from, in.signal)
    //println(ins)
    if ins.values.forall(identity)
    then sendSignal(false)
    else sendSignal(true)

case class Broadcast(name: String, out: List[String]) extends Module:
  override def logic(in: Signal): List[Signal] =
    sendSignal(in.signal)

val moduleRegex = "([%&]?\\S+) -> (.*)".r

//val s = "broadcaster -> a, b, c\n%a -> b\n%b -> c\n%c -> inv\n&inv -> a"
val s = "broadcaster -> a\n%a -> inv, con\n&inv -> b\n%b -> con\n&con -> output"

given ms: Map[String, Module] = s.asStrs
  .map(_.split(" -> ") match {
    case Array(s"%$name", outs) => name -> Flipflop(name, outs.asWords(", ").toList)
    case Array(s"&$name", outs) => name -> Conjunction(name, outs.asWords(", ").toList)
    case Array(name, outs)      => name -> Broadcast(name, outs.asWords(", ").toList)
  })
  .toMap
val cs    = ms.collect { case (name, _: Conjunction) => name }.toSeq
val csIns = cs.map(cn => cn -> ms.collect { case (name, m) if m.out.contains(cn) => name })
csIns.foreach((n, ins) => ms(n).asInstanceOf[Conjunction].addIns(ins.toList))
ms("inv").out

//given signals: mutable.Queue[Signal] = mutable.Queue(("button", false, "broadcaster"))
val pushButtonSignal: Signal = ("button", false, "broadcaster")

type SignalKind = (Int, Int)

extension (sk: SignalKind)
  def +(other: SignalKind): SignalKind =
    (sk._1 + other._1) -> (sk._2 + other._2)
  def product: Long = sk._1.toLong * sk._2

def pushButtonOne(using ms: Map[String, Module]): SignalKind =
  @tailrec
  def step(signalQueue: Queue[Signal], counter: SignalKind = 0 -> 0): SignalKind =
    if signalQueue.isEmpty then counter
    else
      val (sig, newQueue) = signalQueue.dequeue
      val newCnt = if sig.signal then 1 -> 0 else 0 -> 1
      val newSignals = if ms.contains(sig.dest)
        then ms(sig.dest).logic(sig)
        else EmptySignal
      step(newQueue.appendedAll(newSignals), counter + newCnt)
  step(Queue(pushButtonSignal))

pushButtonOne

def pushButton(cnt: Int)(using ms: Map[String, Module]): SignalKind =
  @tailrec
  def rec(cur: Int, counter: SignalKind = 0 -> 0): SignalKind =
    if cur == 0 then counter
    else rec(cur - 1, counter + pushButtonOne)
  rec(cnt)

pushButton(1000).product

AoCLib.math.lcm(Seq(2761, 2793, 2847, 2881))