package line

import line.LineKind.{Dot, Vertical}
import coord.{Pos, given}

import scala.math.{max, min}

enum LineKind:
  case Arbitrary, Vertical, Horizontal, Diagonal, Dot

case class Line(p1: Pos, p2: Pos):
  
  def isVertical: Boolean = p1.x == p2.x
  
  def isHorizontal: Boolean = p1.y == p2.y
  
  def isVH: Boolean = isVertical || isHorizontal
  
  def expand: Seq[Pos] = this match
    case lh if lh.isHorizontal =>
      rangeByCoord(_.x).map(Pos(_,p1.y))
    case lv if this.isVertical =>
      rangeByCoord(_.y).map(Pos(p1.x,_))
    case _ => rangeByCoord(_.x).zip(rangeByCoord(_.y)).map(Pos.apply)
    
  def rangeByCoord(coord: Pos => Int): Range =
    if coord(p2) > coord(p1)
    then coord(p1) to coord(p2)
    else coord(p1) to coord(p2) by -1

  def manhattan: Int = p1 manhattan p2

  def length: Double = (p2.x - p1.x) * (p2.x - p1.x) + (p2.y - p1.y) * (p2.y - p1.y)

  def kind: LineKind =
    if manhattan == 0
    then LineKind.Dot
    else if isVertical
    then LineKind.Vertical
    else if isHorizontal
    then LineKind.Horizontal
    else
      val pd = p2 - p1
      if pd.x.abs == pd.y.abs
      then LineKind.Diagonal
      else LineKind.Arbitrary
