//val s = "0 1 10 99 999"
val s = "125 17"
val sts = s.split(" ")

//val cache: Set[String] = Set.empty

val rules: PartialFunction[String,List[String]] =
  case "0" => List("1")
  case s if s.length % 2 == 0 =>
    val len = s.length
    val left = s.take(len / 2).toLong.toString
    val right = s.drop(len / 2).toLong.toString
    List(left, right)
  case s => List((s.toLong * 2024).toString)

def is(start: Array[String]) = Iterator.iterate(start)(_.flatMap(rules))
is(sts).drop(25).next().length
def unf(start: Array[String]) = Iterator.unfold(start -> Set.empty[String]){ (st, cache) =>
  val res = st.flatMap(rules)
  if res.forall(cache.contains) then None
  else Some(res.length, res -> (cache ++ res.toSet))
}
val s0 = unf(Array("0")).toList
val v0 = s0.size -> s0.last
val s125 = unf(Array("125")).toList
val v125 = s125.size -> s125.last
val s17 =unf(Array("17")).toList
val v17 = s17.size -> s17.last

def cicl(start: String) = Iterator.unfold(Array(start) -> Set.empty[String]){ (st, cache) =>
  val res = st.flatMap(rules)
  if cache.contains(start) then None
  else Some(res.length, res -> (cache ++ res.toSet))
}
cicl("0")
val r0 = rules("0")
val r1 = r0.flatMap(rules)
val r2 = r1.flatMap(rules)
val r3 = r2.flatMap(rules)
val r4 = r3.flatMap(rules)
val r5 = r4.flatMap(rules)
val r6 = r5.flatMap(rules)
val r7 = r6.flatMap(rules)
val r8 = r7.flatMap(rules)
val r9 = r8.flatMap(rules)
val r10 = r9.flatMap(rules)
val r11 = r10.flatMap(rules)
val r12 = r11.flatMap(rules)

is(Array("0")).takeWhile(!_.contains("0")).toList