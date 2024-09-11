package box

import coord.{Pos, given}
import scala.math.Numeric.Implicits.infixNumericOps

case class Box(min: Pos, max: Pos) extends Boxs[Pos, Box](min: Pos, max: Pos):
  override def build: BoxFactory[Pos, Box] = Box

  override def size[C](using num: Numeric[C]): C =
    val d = max - min + Pos(1,1)
    num.fromInt(d.x) * num.fromInt(d.y)

  override def iterator: Iterator[Pos] =
    for
      x <- (min.x to max.x).iterator
      y <- (min.y to max.y).iterator
    yield Pos(x, y)

object Box extends BoxFactory[Pos, Box] {}
