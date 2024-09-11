val s =
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
val ss = s.split("\n")

enum Cube:
  case Red, Green, Blue
import Cube.*

case class Game(id: Int, rs: Array[Map[Cube, Int]])

val gs = ss.map { case s"Game $id: $gs" =>
             id.toInt -> gs
               .split("; ")
               .map(
                 _.split(", ")
                  .map { case s"$cnt $color" =>
                    color -> cnt.toInt
                  }
                  .toMap
               )
           }
           .map(g => g._1 -> g._2.flatten.groupMapReduce(_._1)(_._2)(_ max _))

gs
  .filterNot(_._2.exists { (k, v) =>
    k match
      case "red"   => v > 12
      case "green" => v > 13
      case "blue"  => v > 14
  })
  .map(_._1)
  .sum

gs.map(_._2.values.product).sum
