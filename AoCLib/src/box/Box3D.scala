package box3d

import coord.{Pos3D, given}
import box.*

import scala.math.Numeric.Implicits.infixNumericOps

case class Box3D(min: Pos3D, max: Pos3D) extends Boxs[Pos3D, Box3D](min: Pos3D, max: Pos3D):
  override def build: BoxFactory[Pos3D, Box3D] = Box3D

  override def size[C](using num: Numeric[C]): C =
    val d = max - min + Pos3D(1, 1, 1)
    num.fromInt(d.x) * num.fromInt(d.y) * num.fromInt(d.z)

  override def iterator: Iterator[Pos3D] =
    for
      x <- (min.x to max.x).iterator
      y <- (min.y to max.y).iterator
      z <- (min.z to max.z).iterator
    yield Pos3D(x, y, z)

  def xyIterator: Iterator[Pos3D] =
    for
      x <- (min.x to max.x).iterator
      y <- (min.y to max.y).iterator
    yield Pos3D(x, y, 0)

  def xR: Range = min.x to max.x
  def yR: Range = min.y to max.y
  def zR: Range = min.z to max.z

object Box3D extends BoxFactory[Pos3D, Box3D] {}
