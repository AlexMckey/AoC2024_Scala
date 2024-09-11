val s = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
s.split("\n").map(x => "" + x.find(_.isDigit).get + x.findLast(_.isDigit).get).map(_.toInt).sum

val s2 = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"
  .split("\n")

s2.map(_.reverse)

val nums = "1|2|3|4|5|6|7|8|9|0|one|two|three|four|five|six|seven|eight|nine"
val numsL = nums.split('|')
val numsLR = numsL.map(_.reverse)
val d = "\\d"
val numsRe = (".*(" + d + "|" + nums + ").*").r
val revNumsRe = (".*(" + d + "|" + nums.reverse + ").*").r

s2.map(x => numsL.fold(x)((acc,p) => acc.replaceAll(p,(numsL.indexOf(p)+1).toString)))

s2.map(x =>
  val a = x match
    case numsRe(str) => (numsL.indexOf(str)+1)%10
    case _ => 0
  val b = x.reverse match
    case revNumsRe(str) => (numsLR.indexOf(str)+1)%10
    case _ => 0
  s"$b$a".toInt
)

nums.r.findFirstMatchIn(s2.head).map(_.group(0))
s2.map(x =>
  val l = nums.r.findAllMatchIn(x).map(_.group(0)).toList
  val a = (numsL.indexOf(l.head)+1)%10
  val b = (numsL.indexOf(l.last)+1)%10
  s"$a$b".toInt
)
