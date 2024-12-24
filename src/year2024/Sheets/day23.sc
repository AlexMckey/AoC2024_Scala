import graph.traverse.BFS
import exts.maps.merge
import exts.iterables.groupCount

val s = "kh-tc\nqp-kh\nde-cg\nka-co\nyn-aq\nqp-ub\ncg-tb\nvc-aq\ntb-ka\nwh-tc\nyn-cg\nkh-ub\nta-co\nde-co\ntc-td\ntb-wq\nwh-td\nta-ka\ntd-qp\naq-cg\nwq-ub\nub-vc\nde-ta\nwq-aq\nwq-vc\nwh-yn\nka-de\nkh-ta\nco-tc\nwh-qp\ntb-vc\ntd-yn"

case class Connect(pc1: String, pc2: String)

val cs = s.split("\n").map(_.split("-")).map{ case Array(a,b) => a -> b}
//val mcs = cs.map(c => c.pc1 -> c.pc2).toMap ++ cs.map(c => c.pc2 -> c.pc1).toMap

val cgs = cs.groupMap(_._1)(_._2).merge(cs.groupMap(_._2)(_._1))(_++_)
cgs.values.map(_.size + 1)
val cgss = cgs.keySet.map(cgs(_).flatMap(cgs(_)).groupCount(identity).filter(_._2 > 1).keySet)
cgss.size
cgss.toList.sortBy(_.size).last
cgss.collect{ case set if set.exists(_.startsWith("t")) => set}
  .foreach(println)
val ccgs = cgs.keySet.toList.combinations(3).toList

val conns = s.split("\n").map(_.split("-")).flatMap{ case Array(a,b) => List(a -> b, b -> a)}
val cm = conns.groupMap(_._1)(_._2).map((c,a) => c -> a.toSet)
val res1 =
  (for
    (n, ajcs) <- cm
    n1 <- ajcs
    n2 <- ajcs - n1
    //if cm.contains(n1)
    if cm(n1).contains(n2)
    //if n.startsWith("t") || n1.startsWith("t") || n2.startsWith("t")
  yield Set(n,n1,n2))
    .toList
    .distinct
res1.size
res1.count(_.exists(_.startsWith("t")))

val adjList/*: Map[String, Set[String]]*/ = s
  .split("\n")
  .map(_.split("-"))
  .flatMap{ case Array(a,b) => Seq(a -> b, b -> a)}
  .groupMap(_._1)(_._2)
  .map((n,sns) => n -> sns.toSet)