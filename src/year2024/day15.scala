package year2024.day15

import year2024.DayOf2024
import parse.{*, given}
import grid.{CharGrid, Grid, VectorGrid}
import coord.{GridDir, Pos}
import GridDir.*
import coord.Dir.*

type Maze = CharGrid
case class Input(maze: Maze, moves: Array[Char]):
  def expandedMaze: Maze =
    VectorGrid(maze.map {
      case '@' => "@."
      case '.' => ".."
      case 'O' => "[]"
      case '#' => "##"
    }.toString)

given Read[Input] = Read.product("\n\n")
given Read[Maze] = VectorGrid.charVectorGridReader
//given Read[Array[Char]] = _.replaceAll("\n","").toCharArray
given Read[Array[Char]] = summon[Read[String]].replaceAll(List("\n" -> "")).map(_.toCharArray)

object Day15 extends DayOf2024[Input](15, "Warehouse Woes"):

  extension (maze: CharGrid)
    def GPScore(stone: Char = 'O'): Long =
      maze.findAll(stone).foldLeft(0L): (acc, p) =>
        acc + p.y * 100 + p.x

  case class SimpleState(g: CharGrid, p: Pos):
    private val wall = '#'
    private val stone = 'O'
    private val empty = '.'

    def step(ch: Char): SimpleState =
      val newP = p.toDir(ch.asGridDir)
      g(newP) match
        case `wall` => this
        case `empty` => SimpleState(g.updated(newP, g(p)).updated(p, empty), newP)
        case `stone` =>
          val newState = SimpleState(g, newP).step(ch)
          if newState.p == newP
          then SimpleState(newState.g, p)
          else SimpleState(newState.g.updated(newP, newState.g(p)).updated(p, empty), newP)

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

  override def part1(i: Input): Long =
    val maze = i.maze
    val start = maze.find('@').get
    i.moves
      .foldLeft(SimpleState(maze, start)){ (st, d) => st.step(d) }
      .g.GPScore()

  override def part2(i: Input): Long =
    val expandedMaze = i.expandedMaze
    val start = expandedMaze.find('@').get
    i.moves
      .foldLeft(ExtendedState(expandedMaze, start)) { (st, d) => st.step(d) }
      .g.GPScore('[')

//Day 15: Warehouse Woes
//  parse : 24.7ms
//  part 1: 127.ms -> 1516281
//  part 2: 91.6ms -> 1527969