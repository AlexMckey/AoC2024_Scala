package coord

case class PosHex(x: Int, y: Int, z: Int):
  require(x + y + z == 0)
  override def toString: String = this.show
  
object PosHex:  
  def zero: PosHex = Coord.zero[PosHex]

given posHex: Coord[PosHex] with
  type Item = Int
  extension (v: PosHex)
    def axis: Seq[Int] = Seq(v.x, v.y, v.z)
  extension (a: PosHex)(using N: Numeric[Item])
    override def manhattan(b: PosHex): Item = zip(a, b)(N.minus).map(_ / 2).norm
  def build(xs: Seq[Int]): PosHex = PosHex(xs.head, xs.tail.head, xs.last)
  val size: Int = 3