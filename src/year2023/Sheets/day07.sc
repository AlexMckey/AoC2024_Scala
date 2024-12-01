import AoCLib.exts.*

import scala.util.chaining.*

enum Card:
  case Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace
  override def toString: String = this match
    case Ace => "A"
    case King => "K"
    case Queen => "Q"
    case Jack => "J"
    case Ten => "T"
    case Nine => "9"
    case Eight => "8"
    case Seven => "7"
    case Six => "6"
    case Five => "5"
    case Four => "4"
    case Three => "3"
    case Two => "2"

import Card.*

extension (c: Card)
  def jokerOrdinal: Int =
    if c == Jack
    then 1
    else c.ordinal

def toCard(c: Char): Card = c match
  case 'A' => Ace
  case 'K' => King
  case 'Q' => Queen
  case 'J' => Jack
  case 'T' => Ten
  case c => Card.fromOrdinal(c.asDigit - 2)

enum Kind:
  case HighCard,Pair,TwoPair,ThreeKind,FullHouse,FourKind,FiveKind

import Kind.*

case class Hand(cards: Seq[Card], bid: Int):
  def kind: Kind =
    val counts = cards.groupMapReduce(identity)(_ => 1)(_ + _)
      .values.toSeq.sorted(Ordering[Int].reverse)
    counts match
      case Seq(5) => FiveKind
      case Seq(4,1) => FourKind
      case Seq(3,2) => FullHouse
      case Seq(3,_*) => ThreeKind
      case Seq(2,2,1) => TwoPair
      case Seq(2,_*) => Pair
      case _ => HighCard

  def kindWithJoker: Kind =
    val njCards = cards.filterNot(_ == Jack)
    val maxCard = njCards.groupMapReduce(identity)(_ => 1)(_ + _)
      .toSeq.sortBy(_._2)(Ordering.Int.reverse).head._1
    this.copy(cards = cards
      .map(c => if c == Jack then maxCard else c))
      .kind

object PureOrdering:
  given pureCardOrdering: Ordering[Card] with
    override def compare(x: Card, y: Card): Int =
      x.ordinal.compare(y.ordinal)

  given pureHandOrdering: Ordering[Hand] with
    override def compare(x: Hand, y: Hand): Int =
      val res = x.kind.ordinal.compare(y.kind.ordinal)
      if res != 0
      then res
      else x.cards.zip(y.cards)
        .map(summon[Ordering[Card]].compare)
        .find(_ != 0).get

object JockerOrdering:
  given jokerCardOrdering: Ordering[Card] with
    override def compare(x: Card, y: Card): Int =
      x.jokerOrdinal.compare(y.jokerOrdinal)

  given jokerHandOrdering: Ordering[Hand] with
    override def compare(x: Hand, y: Hand): Int =
      val res = x.kindWithJoker.ordinal.compare(y.kindWithJoker.ordinal)
      if res != 0
      then res
      else x.cards.zip(y.cards)
        .map(summon[Ordering[Card]].compare)
        .find(_ != 0).get


val s = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"
  .asStrs

val hs = s.map{ case s"$cs $bids" => Hand(cs.map(toCard), bids.toInt) }

hs.map(_.cards
  .groupMapReduce(identity)(_ => 1)(_ + _)
  .values.toSeq.sorted(Ordering[Int].reverse))

hs.map(_.kind)

import PureOrdering.given

hs.sorted(PureOrdering.pureHandOrdering)
  .zipWithIndex.map((h,i) => h.bid * (i+1)).sum

val h1 = hs.tail.head
val (jh1, njh1) = h1.cards.partition(_ == Jack)
val maxC = njh1.groupMapReduce(identity)(_ => 1)(_ + _)
  .toSeq.sortBy(_._2)(Ordering.Int.reverse).head._1
h1.copy(cards = h1.cards.map(c => if c == Jack then maxC else c)).kind

hs.sorted(JockerOrdering.jokerHandOrdering)
  .zipWithIndex.map((h,i) => h.bid * (i+1)).sum


import math.Ordering.Implicits.infixOrderingOps
Jack > Queen


