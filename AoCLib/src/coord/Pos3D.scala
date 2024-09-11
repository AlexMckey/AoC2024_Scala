package coord

case class Pos3D(x: Int, y: Int, z: Int):
  override def toString: String = this.show

object Pos3D:
  def zero: Pos3D = Coord.zero[Pos3D]

given pos3D: Coord[Pos3D] with
  type Item = Int
  extension (v: Pos3D)
    def axis: Seq[Int] = Seq(v.x, v.y, v.z)
  def build(xs: Seq[Int]): Pos3D = Pos3D(xs.head, xs.tail.head, xs.last)
  val size: Int = 3
