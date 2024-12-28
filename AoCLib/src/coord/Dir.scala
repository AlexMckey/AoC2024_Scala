package coord

//opaque type Dir(x: Int, y: Int) = Pos(x, y)
//
//object Dir:
//  def apply(p: Pos): Dir = Pos(p.x.sign, p.y.sign)
//  def unapply(d: Dir): Pos = Pos(d.x, d.y)
//  val N = Dir(0, -1)
//  val E = Dir(1, 0)
//  val S = Dir(0, 1)
//  val W = Dir(-1, 0)
//  val NE = Dir(1, -1)
//  val SE = Dir(1, 1)
//  val NW = Dir(-1, -1)
//  val SW = Dir(-1, 1)
//
//  def between(start: Pos, end: Pos): Dir =
//    val delta = end - start
//    Dir(delta.x.sign, delta.y.sign)
//    //Dir.values.find(_.delta == delta).getOrElse(sys.error(s"$start and $end are not neighbours"))
//
//  val axisDirs: Seq[Dir] = List(N, E, S, W)
//  val diagDirs: Seq[Dir] = List(NE, SE, SW, NW)
//  val allDirs: Seq[Dir] = axisDirs ++ diagDirs
//
//import Dir.*
//
//extesion (d: Dir)
//  def turnLeft: Dir = d match
//    case `N` => W
//    case `E` => N
//    case `S` => E
//    case `W` => S
//    case `NE` => NW
//    case `NW` => SW
//    case `SW` => SE
//    case `SE` => NE
//
//  def turnRight: Dir = d match
//    case `N` => E
//    case `E` => S
//    case `S` => W
//    case `W` => N
//    case `NE` => SE
//    case `SE` => SW
//    case `SW` => NW
//    case `NW` => NE
//
//  def flip: Dir = d.turnLeft.turnLeft
//
//  def turn(left: Boolean, times: Int): Dir =
//    if left
//    then (1 to times).foldLeft(d)((d, _) => d.left)
//    else (1 to times).foldLeft(d)((d, _) => d.right)
//
//  def turn(degrees: Int): Dir =
//    turn(left = true, (360 + degrees) % 360 / 90)
//
//extension (p: Pos)
//  def ~(d: Dir): Pos = p + d
//  def ~~(d: Dir): Iterator[Pos] = Iterator.iterate(p)(_ ~ d)
////  def asDir: Dir =
////    Dir.values.map(d => d -> d.delta).find(_._2 == p) match
////      case Some((d, _)) => d
////      case _ => throw new IllegalArgumentException("Неверная позиция для напрвления")
//
//object GridDir:
//  val L: Dir = Dir.W
//  val R: Dir = Dir.E
//  val U: Dir = Dir.N
//  val D: Dir = Dir.S
//
//extension (ch: Char)
//  def asGridDir: Dir = ch match
//    case '^' => U
//    case '>' => R
//    case 'v' => D
//    case '<' => L
//    case _   => throw new IllegalArgumentException("Unknown direction")

enum Dir private(val delta: Pos):
  case N extends Dir(Pos(0, -1))
  case E extends Dir(Pos(1, 0))
  case S extends Dir(Pos(0, 1))
  case W extends Dir(Pos(-1, 0))
  case NE extends Dir(Pos(1, -1))
  case SE extends Dir(Pos(1, 1))
  case NW extends Dir(Pos(-1, -1))
  case SW extends Dir(Pos(-1, 1))
  case Stand extends Dir(Pos.zero)

  def left: Dir = this match
    case N => W
    case E => N
    case S => E
    case W => S
    case NE => NW
    case NW => SW
    case SW => SE
    case SE => NE
    case Stand => Stand

  def right: Dir = this match
    case N => E
    case E => S
    case S => W
    case W => N
    case NE => SE
    case SE => SW
    case SW => NW
    case NW => NE
    case Stand => Stand

  def flip: Dir = left.left

  def turn(left: Boolean, times: Int): Dir =
    if left then (1 to times).foldLeft(this)((d, _) => d.left)
    else (1 to times).foldLeft(this)((d, _) => d.right)

  def turn(degrees: Int): Dir =
    turn(left = true, (360 + degrees) % 360 / 90)

object Dir:
  extension (start: Pos)
    def ~(d: Dir): Pos = start + d.delta
    def ~~(d: Dir): Iterator[Pos] = Iterator.iterate(start)(_ ~ d)
    def asDir: Dir =
      Dir.values.find(_.delta == start) match
        case Some(d) => d
        case _ => throw new IllegalArgumentException("Неверная позиция для направления")
    def between(end: Pos): Dir = (end - start).sign.asDir

  import Dir._
  val axisDirs: Seq[Dir] = List(N,E,S,W)
  val diagDirs: Seq[Dir] = List(NE,SE,SW,NW)
  val allDirs: Seq[Dir] = axisDirs ++ diagDirs

object GridDir:
  val L: Dir = Dir.W
  val R: Dir = Dir.E
  val U: Dir = Dir.N
  val D: Dir = Dir.S
  extension (d: Dir)
    def asDirChar: Char = d match
      case Dir.E => '>'
      case Dir.W => '<'
      case Dir.N => '^'
      case Dir.S => 'v'
      case _ => '.'

  extension (ch: Char)
    def asGridDir: Dir = ch match
      case '^' => U
      case '>' => R
      case 'v' => D
      case '<' => L
      case _   => throw new IllegalArgumentException("Unknown direction")
    //export Dir.*
