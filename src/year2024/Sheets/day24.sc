import exts.*

import scala.annotation.tailrec

//val s = "x00: 1\nx01: 1\nx02: 1\ny00: 0\ny01: 1\ny02: 0\n\nx00 AND y00 -> z00\nx01 XOR y01 -> z01\nx02 OR y02 -> z02"
val s = "x00: 1\nx01: 0\nx02: 1\nx03: 1\nx04: 0\ny00: 1\ny01: 1\ny02: 1\ny03: 1\ny04: 1\n\nntg XOR fgs -> mjb\ny02 OR x01 -> tnw\nkwq OR kpj -> z05\nx00 OR x03 -> fst\ntgd XOR rvg -> z01\nvdt OR tnw -> bfw\nbfw AND frj -> z10\nffh OR nrd -> bqk\ny00 AND y03 -> djm\ny03 OR y00 -> psh\nbqk OR frj -> z08\ntnw OR fst -> frj\ngnj AND tgd -> z11\nbfw XOR mjb -> z00\nx03 OR x00 -> vdt\ngnj AND wpb -> z02\nx04 AND y00 -> kjc\ndjm OR pbm -> qhw\nnrd AND vdt -> hwm\nkjc AND fst -> rvg\ny04 OR y02 -> fgs\ny01 AND x02 -> pbm\nntg OR kjc -> kwq\npsh XOR fgs -> tgd\nqhw XOR tgd -> z09\npbm OR djm -> kpj\nx03 XOR y03 -> ffh\nx00 XOR y04 -> ntg\nbfw OR bqk -> z06\nnrd XOR fgs -> wpb\nfrj XOR qhw -> z04\nbqk OR frj -> z07\ny03 OR x01 -> nrd\nhwm AND bqk -> z03\ntgd XOR rvg -> z12\ntnw OR pbm -> gnj"

val Array(wireStr,gateStr) = s.split("\n\n")

type Wire = String

enum GateType{
  case AND, OR, XOR
}

import GateType.*

type State = Map[Wire,Int]

case class Gate(g: GateType, w1: Wire, w2: Wire, out: Wire) {
  def exec(ws: State): State = {
    val (i1, i2) = ws(w1) -> ws(w2)
    val res = g match {
          case AND => i1 & i2
          case OR => i1 | i2
          case XOR => i1 ^ i2
        }
    ws.updated(out, res)
  }
}

val wires: State = wireStr.split("\n")
  .map{ case s"$wire: ${I(signal)}" => wire -> signal }.toMap
val gates: List[Gate] = gateStr.split("\n")
  .map{ case s"$w1 $gate $w2 -> $out" => Gate(GateType.valueOf(gate), w1, w2, out)}.toList

val (toExec, waits) = gates.partition(g => wires.contains(g.w1) && wires.contains(g.w2))
val newState = toExec.foldLeft(wires)((state,g) => g.exec(state))
val (toExec1, waits1) = waits.partition(g => newState.contains(g.w1) && newState.contains(g.w2))
val newState1 = toExec1.foldLeft(newState)((state,g) => g.exec(state))
val (toExec2, waits2) = waits1.partition(g => newState1.contains(g.w1) && newState1.contains(g.w2))
val newState2 = toExec2.foldLeft(newState1)((state,g) => g.exec(state))
val (toExec3, waits3) = waits2.partition(g => newState2.contains(g.w1) && newState2.contains(g.w2))

@tailrec
def calcStableState(gates: List[Gate])(wires: State): State = {
  if gates.isEmpty
  then wires
  else
    val (toExec, waits) = gates.partition(g => wires.contains(g.w1) && wires.contains(g.w2))
    calcStableState(waits)(toExec.foldLeft(wires)((state,g) => g.exec(state)))
}

val res = calcStableState(gates)(wires)
res.filter(_._1.startsWith("z")).toList.sortBy(_._1)(Ordering[String].reverse).map(_._2).mkString.toIntRadix(2)

extension (st: State){
  def outBin: String = {
    st.filter(_._1.startsWith("z"))
      .toList
      .sortBy(_._1)(Ordering[String].reverse)
      .map(_._2)
      .mkString
  }
  def outNum: Long = {
    st.outBin.toLongRadix(2)
  }
}

def calcSum(gates: List[Gate])(x: String, y: String): String = {
  val xs = x.reverse.zipWithIndex.map((char,i) => f"x$i%02d" -> char.asDigit)
  val ys = y.reverse.zipWithIndex.map((char,i) => f"y$i%02d" -> char.asDigit)
  val inputs = (xs ++ ys).toMap
  calcStableState(gates)(inputs).outBin
}
calcSum(gates)("00000","00000")
calcSum(gates)("00001","00000")
calcSum(gates)("00000","00001")
calcSum(gates)("00001","00001")

calcSum(gates)("00010","00000")
calcSum(gates)("00000","00010")
calcSum(gates)("00010","00010")

calcSum(gates)("00100","00000")
calcSum(gates)("00000","00100")
calcSum(gates)("00100","00100")

calcSum(gates)("01000","00000")
calcSum(gates)("00000","01000")
calcSum(gates)("01000","01000")

val gs = "qgv OR dsf -> mjm\nshj AND wsr -> ftr\njcf OR wgg -> chb\njbp AND grd -> tcw\nrbm OR sck -> dtj\nqjb OR qqm -> vdr\nktf OR pvt -> tjg\nx32 XOR y32 -> wnj\nnbc AND wjv -> tjm\ny42 XOR x42 -> ksk\ny31 XOR x31 -> mqk\ny32 AND x32 -> jfp\ngrd XOR jbp -> z36\ny39 XOR x39 -> bmb\nvbj XOR chb -> z04\ntqv XOR www -> z40\nx34 AND y34 -> sck\ny07 AND x07 -> dsf\ny12 XOR x12 -> qvh\nfnt AND bnk -> pvt\nx10 XOR y10 -> fnt\nwgw AND nph -> hjt\nx30 XOR y30 -> bgs\nbdm XOR hvn -> z37\nmtg XOR vnm -> z06\ngdn OR pck -> wsr\nttr OR nfj -> fbv\njtn AND vtk -> wqs\nx08 XOR y08 -> qjb\ndmm OR mmr -> nhk\nqqd AND cph -> bqd\ntqv AND www -> nnr\nggg XOR kbg -> z03\nhff AND gdg -> twb\nnqm OR whj -> ghb\nx14 AND y14 -> nfj\ncrv XOR cvf -> z25\nftc OR jjn -> z45\nbnk XOR fnt -> z10\ny30 AND x30 -> mmr\nqtv AND dnj -> gbg\nx23 AND y23 -> vgr\nndd OR ncb -> fhg\nx33 AND y33 -> qgm\nrrh OR dqs -> gdp\ny40 XOR x40 -> www\nx41 XOR y41 -> wvq\njtn XOR vtk -> z20\ntpd OR djn -> qvc\ny18 AND x18 -> jdr\npnd OR hwf -> vtk\ny26 AND x26 -> vqh\nvcs XOR dtj -> jbp\nx03 AND y03 -> wgg\nrjv OR fgf -> brq\nvjn AND tjg -> trg\ncph XOR qqd -> z05\njbr AND ncd -> mvb\nqvh AND wmd -> fbn\nx25 XOR y25 -> cvf\nx39 AND y39 -> rvc\ny44 AND x44 -> ftc\ny36 XOR x36 -> grd\ny03 XOR x03 -> kbg\nvvj XOR bqv -> z27\nvbj AND chb -> kqr\ntwb OR dtd -> stw\nmnv XOR hcg -> z13\ny26 XOR x26 -> dnj\nrkd OR rmp -> wrw\ny22 XOR x22 -> tdc\nwvq AND tkt -> jsc\nmjm XOR gvw -> z08\nhvn AND bdm -> pck\ncbf XOR vdr -> z09\nhdb OR hmb -> qtv\ndvs OR mvd -> hwm\nmqk AND nhk -> ncb\nrfj OR jgc -> bst\nwrw XOR hwh -> z28\ngdp XOR stm -> z07\nstm AND gdp -> qgv\nhff XOR gdg -> z29\ny44 XOR x44 -> vbd\ntqq OR jjc -> tpn\nqgm OR mvb -> cqg\ndkg XOR ghb -> z02\nwrw AND hwh -> pkq\nhwm AND tdc -> z22\nx02 AND y02 -> gws\njfp OR mpd -> ncd\ny11 AND x11 -> smr\nhvd AND bmb -> vts\nkqr OR jsj -> qqd\ny24 XOR x24 -> wgw\nstw XOR bgs -> z30\ntpn AND wcn -> tpd\ny19 AND x19 -> pnd\ny38 AND x38 -> dkn\npkq OR vbc -> hff\ny31 AND x31 -> ndd\nwwc AND nvv -> whj\ny14 XOR x14 -> drk\nbst AND pkj -> jjc\nsht OR fhh -> bnk\nx05 XOR y05 -> cph\nvts OR rvc -> tqv\nx04 AND y04 -> jsj\nwjv XOR nbc -> z23\ny17 XOR x17 -> wcn\nx04 XOR y04 -> vbj\ny01 AND x01 -> nqm\nx01 XOR y01 -> wwc\nvdr AND cbf -> sht\njhm AND wst -> dvs\ntpv XOR drk -> z14\ncjm XOR rws -> z19\nbqd OR hns -> mtg\nx20 XOR y20 -> jtn\nx08 AND y08 -> gvw\ncbc XOR cqg -> z34\nrgt AND fbv -> rfj\nwmd XOR qvh -> z12\nx23 XOR y23 -> wjv\ny35 AND x35 -> ppf\nx43 AND y43 -> trf\nfhg XOR wnj -> z32\nx12 AND y12 -> cmn\ny00 AND x00 -> nvv\ntcw OR jms -> hvn\nnhk XOR mqk -> z31\nx07 XOR y07 -> stm\nvgr OR tjm -> nph\ndrk AND tpv -> ttr\nx09 AND y09 -> fhh\nx22 AND y22 -> snh\nwgw XOR nph -> z24\nmtg AND vnm -> rrh\nqvc AND mms -> tps\nx06 XOR y06 -> vnm\ntps OR jdr -> cjm\nfbv XOR rgt -> jgc\ny41 AND x41 -> wfd\nx19 XOR y19 -> rws\nhcg AND mnv -> cfk\nwsr XOR shj -> z38\nbrq AND jpt -> wtr\ncjm AND rws -> hwf\ny25 AND x25 -> hmb\ny02 XOR x02 -> dkg\ny40 AND x40 -> qsg\nbqv AND vvj -> rmp\nhvd XOR bmb -> z39\ny35 XOR x35 -> vcs\nx42 AND y42 -> rjv\nx37 AND y37 -> gdn\ntjg XOR vjn -> z11\nqtv XOR dnj -> z26\njhm XOR wst -> z21\nnwp OR gws -> ggg\ny00 XOR x00 -> z00\nmrq OR wqs -> wst\nx13 AND y13 -> rsq\nftr OR dkn -> hvd\njbr XOR ncd -> z33\nx05 AND y05 -> hns\nrsq OR cfk -> tpv\nmms XOR qvc -> z18\nx27 AND y27 -> rkd\ncrv AND cvf -> hdb\ncqg AND cbc -> rbm\npkj XOR bst -> z16\nx28 AND y28 -> vbc\njsc OR wfd -> ngw\nmjm AND gvw -> qqm\nwnj AND fhg -> mpd\ny15 XOR x15 -> rgt\nx20 AND y20 -> mrq\ntkt XOR wvq -> z41\ny13 XOR x13 -> mnv\ny24 AND x24 -> nqr\nwtr OR trf -> jvs\nggg AND kbg -> jcf\nx36 AND y36 -> jms\nx29 AND y29 -> dtd\nksk AND ngw -> fgf\nx11 XOR y11 -> vjn\ntpn XOR wcn -> z17\ny43 XOR x43 -> jpt\nvbd AND jvs -> jjn\nnnr OR qsg -> tkt\nx27 XOR y27 -> bqv\nx28 XOR y28 -> hwh\nx09 XOR y09 -> cbf\nvcs AND dtj -> qrg\nx34 XOR y34 -> cbc\nghb AND dkg -> nwp\ny37 XOR x37 -> bdm\nx38 XOR y38 -> shj\nhjt OR nqr -> crv\nvbd XOR jvs -> z44\nfbn OR cmn -> hcg\nx06 AND y06 -> dqs\ny15 AND x15 -> z15\nx33 XOR y33 -> jbr\ny21 XOR x21 -> jhm\nsnh OR drg -> nbc\ny18 XOR x18 -> mms\nvqh OR gbg -> vvj\ny21 AND x21 -> mvd\nqrg OR ppf -> z35\ny10 AND x10 -> ktf\nx16 AND y16 -> tqq\ntdc XOR hwm -> drg\nwwc XOR nvv -> z01\nx17 AND y17 -> djn\njpt XOR brq -> z43\nsmr OR trg -> wmd\nx16 XOR y16 -> pkj\nx29 XOR y29 -> gdg\nbgs AND stw -> dmm\nngw XOR ksk -> z42"
  .split("\n")
  .map{ case s"$w1 $gate $w2 -> $out" => Gate(GateType.valueOf(gate), w1, w2, out)}.toList

val numIn = "01"

val g1 = gs.find(g => g.w1 == "x04").get
val g2 = gs.find(g => g.w2 == "x04").get
val g3 = gs.find(g => g.w1 == "y04").get
val g4 = gs.find(g => g.w2 == "y04").get
val inpChecks = g1 == g4 && g2 == g3
val (l2a,l2x) = if g1.g == AND then g1 -> g2 else g2 -> g1
val l3a = gs.find(g => g.g == AND && (g.w1 == l2x.out || g.w2 == l2x.out)).get
val ciw = if l3a.w1 == l2x.out then l3a.w2 else l3a.w1
val l2o = gs.find(g => if numIn == "01" then g.g == AND else g.g == OR && (g.w1 == ciw || g.w2 == ciw))
val cinChecks = l2o.nonEmpty
val l3o = gs.find(g => g.g == OR && ((g.w1 == l2a.out && g.w2 == l3a.out) || (g.w2 == l2a.out && g.w1 == l3a.out)))
val conChecks = l3o.nonEmpty
val cow = l3o.get.out
val go = gs.find(g => g.g == XOR && ((g.w1 == l2x.out && g.w2 == ciw) || (g.w1 == ciw && g.w2 == l2a.out)))
val outChecks = go.nonEmpty
val checkAll = inpChecks && cinChecks && conChecks && outChecks

def checkAdder(gs: List[Gate])(numIn: String): (Boolean, List[Gate]) = {
  val g1 = gs.filter(g => g.w1 == s"x$numIn")
  val g2 = gs.filter(g => g.w2 == s"x$numIn")
  val g3 = gs.filter(g => g.w1 == s"y$numIn")
  val g4 = gs.filter(g => g.w2 == s"y$numIn")
  val lxy = (g1 ++ g2 ++ g3 ++ g4).distinct
  println(s"xy$numIn: ${List(g1, g2, g3, g4)}")
  val inpChecks = g1 == g4 && g2 == g3
  if !inpChecks then false -> List(g1, g2, g3, g4)
  val (l2a, l2x) = if lxy.head.g == AND then lxy.head -> lxy.last else lxy.last -> lxy.head
  println(s"l2a_x: ${List(l2a, l2x)}")
  val l3a = gs.find(g => g.g == AND && (g.w1 == l2x.out || g.w2 == l2x.out)).get
  println(s"l3a: $l3a")
  val ciw = if l3a.w1 == l2x.out then l3a.w2 else l3a.w1
  println(s"ciw: $ciw")
  val l2o = gs.find(g => (if numIn == "01" then g.g == AND else g.g == OR) && g.out == ciw)
  println(s"l2o: $l2o")
  val cinChecks = l2o.nonEmpty
  if !cinChecks then false -> List(l2a, l2x, l3a)
  val l3o = gs.find(g => g.g == OR && ((g.w1 == l2a.out && g.w2 == l3a.out) || (g.w2 == l2a.out && g.w1 == l3a.out)))
  println(s"l3o: $l3o")
  val conChecks = l3o.nonEmpty
  if !conChecks then false -> List(l3o)
  val cow = l3o.get.out
  println(s"cow: $cow")
  val go = gs.find(g => g.g == XOR && ((g.w1 == l2x.out && g.w2 == ciw) || (g.w1 == ciw && g.w2 == l2x.out)))
  println(s"z$numIn: $go")
  val outChecks = go.nonEmpty
  if !outChecks then false -> List(go)
  val checkAll = inpChecks && cinChecks && conChecks && outChecks
  checkAll -> List.empty
}

//checkAdder(gs)("22")
//val checkAllAdders = (1 to 44).map(i => f"$i%02d").map(checkAdder(gs))
//checkAllCarryOn.forall(_._1)

Seq("z22", "drg", "jgc", "z15", "qjb", "gvw", "jbp", "z35").sorted.mkString(",")

calcSum(gs)("00000","00000")
calcSum(gs)("00001","00000")
calcSum(gs)("00000","00001")
calcSum(gs)("00001","00001")

calcSum(gs)("00010","00000")
calcSum(gs)("00000","00010")
calcSum(gs)("00010","00010")

calcSum(gs)("00100","00000")
calcSum(gs)("00000","00100")
calcSum(gs)("00100","00100")

calcSum(gs)("01000","00000")
calcSum(gs)("00000","01000")
calcSum(gs)("01000","01000")

//def checkBitAdder(gates: List[Gate])(bit: Int): Seq[(Int, Boolean)] =
//  val allBits =
//    for
//      x <- 0 to 1
//      y <- 0 to 1
//      xs = f"x$bit%02d"
//      ys = f"y$bit%02d"
//    yield (x + y) -> Map(xs -> x, ys -> y)
//  allBits.map{(expected,xys) =>
//    val res = calcState(gates)(xys).outNum
//    expected -> (res == expected << bit)
//  }
//
////checkBitAdder(gs)(0)
//val bit = 1
//val allBits =
//  for
//    x <- 0 to 1
//    y <- 0 to 1
//    xs = f"x$bit%02d"
//    ys = f"y$bit%02d"
//  yield (x + y) -> Map(xs -> x, ys -> y)
//
//calcState(gs)(allBits(0)._2).outNum