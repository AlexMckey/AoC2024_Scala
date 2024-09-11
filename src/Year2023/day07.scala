package year2023.day07

import puzzle.Puzzle
import parse.{*, given}
import exts.*
import exts.iterables.*

import scala.math.Ordering.Implicits.*

enum Card:
  case Joker, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace

extension (s: Char)
  def asCard: Card =
    s match
      case 'A' => Ace
      case 'K' => King
      case 'Q' => Queen
      case 'J' => Jack
      case 'T' => Ten
      case c => Card.fromOrdinal(c.asDigit - 1)

import Card.*

given Ordering[Card] with
  override def compare(x: Card, y: Card): Int = x.ordinal.compare(y.ordinal)

enum Kind:
  case HighCard, Pair, TwoPair, ThreeKind, FullHouse, FourKind, FiveKind

import Kind.*

given Ordering[Kind] with
  override def compare(x: Kind, y: Kind): Int = x.ordinal.compare(y.ordinal)

object Kind:
  def byCountCards(cnt: Int): Kind = cnt match
    case 1 => HighCard
    case 2 => Pair
    case 3 => ThreeKind
    case 4 => FourKind
    case 5 => FiveKind

case class Hand(cards: Seq[Card], bid: Int):
  def replaceAll(c1: Card, c2: Card): Hand =
    copy(cards.map(c => if c == c1 then c2 else c))

  def kind: Kind =
    val (jokers, other) = cards.partition(_ == Joker)
    if other.isEmpty then FiveKind
    else
      val counts = other.groupCount(identity)
        .values.toSeq.sorted(Ordering[Int].reverse)
      (counts, jokers.length) match
        case (Seq(2, 2, 1), 0) => TwoPair
        case (Seq(3, 2), 0) => FullHouse
        case (Seq(2, 2), 1) => FullHouse
        case (Seq(x, _*), cnt) => Kind.byCountCards(x + cnt)
        
type I = Seq[Hand]

given Read[I] with 
  override def read(input: String): I =
    input.asStrs.map(str =>
      val Array(cards, bids) = str.split(" ")
      Hand(cards.map(_.asCard), bids.toInt))

object Day07 extends Puzzle[I](2023, 7, "Camel Cards"):

  def part(hands: I): Int =
    hands.sorted(Ordering.by(h => (h.kind, h.cards)))
      .zipWithIndex.map((h, i) => h.bid * (i + 1)).sum

  override def part1(hands: I): Int =
    part(hands)

  override def part2(hands: I): Int =
    val substJoker = hands.map(_.replaceAll(Jack, Joker))
    part(substJoker)

//Day 7: Camel Cards
//  prep: 51.8ms
//  part 1: 290.ms - 253954294
//  part 2: 310.ms - 254837398
