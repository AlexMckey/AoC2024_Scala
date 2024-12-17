import exts.repeated

val s = "Register A: 729\nRegister B: 0\nRegister C: 0\n\nProgram: 0,1,5,4,3,0"
val Array(rs, ps) = s.split("\n\n")
val regs: Map[String,Int] = rs.split("\n").map{
  case s"Register $ch: $num" => ch -> num.toInt
}.toMap
val prog = ps.split(" ").last.split(",").map(_.toInt)

enum Op{ case Adv, Bxl, Bst, Jnz, Bxc, Out, Bdv, Cdv }

def power2(x: Int): Int = 1 << x
def div(x: Int, y: Int): Int = x >> (y-1)

power2(5)
div(3,2)

import Op.*

case class CPU(regs: Map[String, Int], ip: Int = 0, out: StringBuilder = StringBuilder()){
  def Output: String = out.tail.toString()
  def combo(arg: Int): Int = {
    arg match {
      case 0 | 1 | 2 | 3 => arg
      case 4 => regs("A")
      case 5 => regs("B")
      case 6 => regs("C")
      case 7 => throw Exception("Invalid combo operand")
    }
  }
  def exec(prog: Array[Int]): CPU = {
    if (ip < prog.length) {
      val Array(op, arg) = prog.slice(ip, ip + 2)
      //println(s"op: $op, arg:$arg, cpu:$this")
      step(op, arg).exec(prog)
    } else this
  }
  def step(op: Int, arg: Int): CPU = {
    Op.fromOrdinal(op & 7) match {
      case Adv =>
        val res = regs("A") >> combo(arg)
        CPU(regs.updated("A", res), ip + 2, out)
      case Bxl =>
        val res = regs("B") ^ arg
        CPU(regs.updated("B", res), ip + 2, out)
      case Bst =>
        val res = combo(arg) & 7
        CPU(regs.updated("B", res), ip + 2, out)
      case Jnz =>
        if regs("A") == 0
        then CPU(regs, ip + 2, out)
        else CPU(regs, arg, out)
      case Bxc =>
        val res = regs("B") ^ regs("C")
        CPU(regs.updated("B", res), ip + 2, out)
      case Out =>
        val a = combo(arg) & 7
        CPU(regs, ip + 2, out.append(",").append(a))
      case Bdv =>
        val res = regs("A") >> combo(arg)
        CPU(regs.updated("B", res), ip + 2, out)
      case Cdv =>
        val res = regs("A") >> combo(arg)
        CPU(regs.updated("C", res), ip + 2, out)
    }
  }
}

val progPairs = prog.grouped(2).toList

2 & 7

Op.fromOrdinal(2 & 7)

CPU(Map("C" -> 9)).step(2,6).regs("B")
CPU(Map("B" -> 29)).step(1,7).regs("B")
CPU(Map("B" -> 2024, "C" -> 43690)).step(4,0).regs("B")

val p1 = "5,0,5,1,5,4".split(",").map(_.toInt).grouped(2).map{case Array(a,b) => a -> b}.toList
p1.foldLeft(CPU(Map("A" -> 10)))((cpu, pair) => cpu.step.tupled(pair)).out

val p2 = "0,1,5,4,3,0".split(",").map(_.toInt)
p2.slice(0,2)
CPU(Map("A" -> 2024)).exec(p2).Output

val r3 = Map("A" -> 117440)
val p3 = "0,3,5,4,3,0".split(",").map(_.toInt)
val r4 = CPU(r3).exec(p3).Output