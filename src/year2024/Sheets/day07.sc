val s = "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20"

enum Ops:
  case Mul
  case Plus
import Ops.*

def calc(op: Ops, a: Long, b: Long): Long = op match
  case Mul => a * b
  case Plus => a + b

case class Equation(res: Long, args: List[Long]):
  def checkEq: Boolean =
    def rec(accs: List[Long], args: List[Long]): Boolean =
      //println(s"accs: $accs")
      //println(s"args: $args")
      //val acc = accs.head
      //if acc > res then false
      //else
      if args.isEmpty then accs.contains(res)
      else
        val v = args.head
        val rs = accs.flatMap(acc => Ops.values.map(op => calc(op, acc, v)))
        //println(s"rs: ${rs.toList}")
        rec(rs, args.tail)
    rec(List(args.head), args.tail)

val eqs = s.split("\n").map{
  case s"$res: $args" => Equation(res.toLong, args.split(" ").map(_.toLong).toList)
}

eqs.count(_.checkEq)