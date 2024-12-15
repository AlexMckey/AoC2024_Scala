package coord

enum Dir private(val delta: Pos):
  case N extends Dir(Pos(0, -1))
  case E extends Dir(Pos(1, 0))
  case S extends Dir(Pos(0, 1))
  case W extends Dir(Pos(-1, 0))
  case NE extends Dir(Pos(1, -1))
  case SE extends Dir(Pos(1, 1))
  case NW extends Dir(Pos(-1, -1))
  case SW extends Dir(Pos(-1, 1))

  def left: Dir = this match
    case N => W
    case E => N
    case S => E
    case W => S
    case NE => NW
    case NW => SW
    case SW => SE
    case SE => NE

  def right: Dir = this match
    case N => E
    case E => S
    case S => W
    case W => N
    case NE => SE
    case SE => SW
    case SW => NW
    case NW => NE

  def flip: Dir = left.left

  def turn(left: Boolean, times: Int): Dir =
    if left then (1 to times).foldLeft(this)((d, _) => d.left)
    else (1 to times).foldLeft(this)((d, _) => d.right)

  def turn(degrees: Int): Dir =
    turn(left = true, (360 + degrees) % 360 / 90)

object Dir:
  def between(start: Pos, end: Pos): Dir =
    val delta = end - start
    Dir.values.find(_.delta == delta).getOrElse(sys.error(s"$start and $end are not neighbours"))

  extension (p: Pos)
    def ~(d: Dir): Pos = p + d.delta
    def ~~(d: Dir): Iterator[Pos] = Iterator.iterate(p)(_ ~ d)
    def asDir: Dir =
      Dir.values.map(d => d -> d.delta).find(_._2 == p) match
        case Some((d, _)) => d
        case _ => throw new IllegalArgumentException("Неверная позиция для напрвления")

  import Dir._
  val axisDirs: Seq[Dir] = List(N,E,S,W)
  val diagDirs: Seq[Dir] = List(NE,SE,SW,NW)
  val allDirs: Seq[Dir] = axisDirs ++ diagDirs

object GridDir:
  val L: Dir = Dir.W
  val R: Dir = Dir.E
  val U: Dir = Dir.N
  val D: Dir = Dir.S

  extension (ch: Char)
    def asGridDir: Dir = ch match
      case '^' => U
      case '>' => R
      case 'v' => D
      case '<' => L
      case _   => throw new IllegalArgumentException("Unknown direction")
    //export Dir.*
