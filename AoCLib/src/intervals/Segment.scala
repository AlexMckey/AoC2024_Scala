package intervals

trait Segment:
  def contains(p: Int): Boolean
  def splitAt(p: Int): (Segment, Segment)
  def size: Int

object Segment:
  case class Region(low: Int, high: Int) extends Segment:
    def contains(p: Int): Boolean = p >= low && p <= high
    def splitAt(p: Int): (Segment, Segment) =
      if contains(p)
      then Region(low, p - 1) -> Region(p, high)
      else if p > high
      then this  -> Empty
      else Empty -> this
    def size: Int = high - low + 1

  object Empty extends Segment:
    def contains(p: Int): Boolean           = false
    def splitAt(p: Int): (Segment, Segment) = Empty -> Empty
    def size: Int                           = 0
