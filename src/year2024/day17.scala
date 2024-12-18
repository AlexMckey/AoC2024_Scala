package year2024.day17

import year2024.DayOf2024
import parse.{*, given}

case class Regs(var a: Long = 0L, var b: Long = 0L, var c: Long = 0L)
case class CPU(regs: Regs, ip: Int = 0, out: List[Int] = List.empty):
  private def combo(arg: Int): Long =
    arg match
      case 4 => regs.a
      case 5 => regs.b
      case 6 => regs.c
      case _ => arg

  def exec(prog: List[Int]): CPU =
    if prog.indices.contains(ip)
    then
      val List(op, arg) = prog.slice(ip, ip + 2)
      step(op, arg).exec(prog)
    else this

  private def step(op: Int, arg: Int): CPU =
    op & 7 match
      case 0 => CPU(regs.copy(a = regs.a >> combo(arg)), ip + 2, out)
      case 1 => CPU(regs.copy(b = regs.b ^ arg), ip + 2, out)
      case 2 => CPU(regs.copy(b = combo(arg) & 7), ip + 2, out)
      case 3 => CPU(regs, if regs.a == 0 then ip + 2 else arg, out)
      case 4 => CPU(regs.copy(b = regs.b ^ regs.c), ip + 2, out)
      case 5 => CPU(regs, ip + 2, out :+ (combo(arg).toInt & 7))
      case 6 => CPU(regs.copy(b = regs.a >> combo(arg)),  ip + 2, out)
      case 7 => CPU(regs.copy(c =  regs.a >> combo(arg)), ip + 2, out)

case class Input(regs: Regs, prog: List[Int]):
  def findQuine(idx: Int, acc: Long): Option[Long] =
    def finder(octet: Int): Option[Long] =
      val candidate = acc * 8 + octet
      val regs = Regs(a = candidate)
      if CPU(regs).exec(prog).out == prog.toList.drop(idx) then
        if idx == 0 then Some(candidate)
        else findQuine(idx - 1, candidate)
      else None

    (0 until 8).toList.map(finder).collectFirst { case Some(a) => a }

given Read[Input] = Read.product("\n\n")
given Read[List[Int]] = Read.ints
given Read[Regs] = Read.product(""".+: (\d+)""".r)

object Day17 extends DayOf2024[Input](17, "Chronospatial Computer"):

  override def part1(i: Input): String =
    CPU(i.regs).exec(i.prog).out.mkString(",")

  override def part2(i: Input): Long =
    i.findQuine(i.prog.length - 1, 0L).get

//Day 17: Chronospatial Computer
//  parse : 19.9ms
//  part 1: 4.22ms -> 1,5,3,0,2,5,2,5,3
//  part 2: 35.2ms -> 108107566389757