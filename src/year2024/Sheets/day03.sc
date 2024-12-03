import scala.annotation.tailrec
import scala.util.matching.Regex.Match

//val s = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
val s = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
val r = "mul\\((\\d{1,3}),(\\d{1,3})\\)|do\\(\\)|don't\\(\\)".r
val ms = r.findAllMatchIn(s).toList
ms.filter(_.matched.startsWith("mul")).map(_.subgroups)
  .map(_.map(_.toInt).product).sum
  //.map(gm => gm.group(1) -> gm.group(2))

@tailrec
def process(ms: List[Match], acc: Int = 0, isMul: Boolean = true): Int =
  if ms.isEmpty then acc
  else
    val m = ms.head
    if m.matched.startsWith("do()") then process(ms.tail, acc, true)
    else if m.matched.startsWith("don't") then process(ms.tail, acc, false)
    else if m.matched.startsWith("mul") && isMul then
      val v = m.subgroups.map(_.toInt).product
      process(ms.tail, acc + v, isMul)
    else process(ms.tail, acc, isMul)

process(ms)