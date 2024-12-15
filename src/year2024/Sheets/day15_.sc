import grid.{CharGrid, Direction, VectorGrid}
import coord.{Dir, GridDir, Pos}
import GridDir.*
import coord.Dir.*

import scala.annotation.tailrec

//val s = "########\n#..O.O.#\n##@.O..#\n#...O..#\n#.#.O..#\n#...O..#\n#......#\n########\n\n<^^>>>vv<v>>v<<"
val s = "##########\n#..O..O.O#\n#......O.#\n#.OO..O.O#\n#..O@..O.#\n#O#..O...#\n#O..O..O.#\n#.OO.O.OO#\n#....O...#\n##########\n\n<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\nvvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\nv^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
//val s = "#######\n#...#.#\n#.....#\n#..OO@#\n#..O..#\n#.....#\n#######\n\n<vv<<^^<<^^"
val Array(mazes, movess) = s.split("\n\n")
val ms = movess.replaceAll("\n","").toCharArray
val g = VectorGrid(mazes)
val oldStart = g.find('@').get

extension (mz: CharGrid)
  def GPScore(wall: Char = 'O'): Long =
    mz.findAll(wall).foldLeft(0L): (acc, p) =>
      acc + p.y * 100 + p.x

val newG = VectorGrid(g.map{
  case '@' => "@."
  case '.' => ".."
  case 'O' => "[]"
  case '#' => "##"
}.toString)

val newStart = newG.find('@').get

VectorGrid("##########\n##...[]...\n##........")
  .GPScore('[')

case class ExtendedState(g: CharGrid, p: Pos):
  private val wall = '#'
  private val leftBox = '['
  private val rightBox = ']'
  private val empty = '.'

  def step(ch: Char): ExtendedState =
    val newP = p.toDir(ch.asGridDir)

    def moveH: ExtendedState =
      val newState = ExtendedState(g, newP).step(ch)
      if newState.p == newP
      then this
      else ExtendedState(newState.g.updated(newP, newState.g(p)).updated(p, empty), newP)

    def moveV(otherSideBox: Pos): ExtendedState =
      val newState = ExtendedState(g, newP).step(ch)
      if newState.p == newP
      then this
      else
        val otherSideBoxState = ExtendedState(newState.g, otherSideBox).step(ch)
        if otherSideBoxState.p == otherSideBox
        then this
        else
          val resG = otherSideBoxState.g
            .updated(newP, otherSideBoxState.g(p))
            .updated(p, empty)
            .updated(otherSideBox, otherSideBoxState.g(otherSideBox))
            .updated(otherSideBox, empty)
          ExtendedState(resG, newP)

    g(newP) match
      case `wall` => this
      case `empty` => ExtendedState(g.updated(newP, g(p)).updated(p, empty), newP)
      case `leftBox` =>
        if ch == '<' || ch == '>'
        then moveH
        else moveV(newP.toDir(R))
      case `rightBox` =>
        if ch == '<' || ch == '>'
        then moveH
        else moveV(newP.toDir(L))

val resG = ms.foldLeft(ExtendedState(newG, newStart)) { (st, d) => st.step(d) }
resG.g.GPScore('[')