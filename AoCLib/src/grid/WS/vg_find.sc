val s = "123\n021\n412"
val vg = s.split("\n").map(_.toCharArray)
vg.zipWithIndex.map((r,y) =>
    r.zipWithIndex.collectFirst{
      case (c,x) if c == '2' => y -> x })
  .collectFirst{
    case Some(p) => p
  }

val (u,d) = vg.splitAt(2)
u ++ d.tail

vg.transpose