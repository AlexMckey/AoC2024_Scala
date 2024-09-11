package exts.iterators

import java.util.NoSuchElementException
import scala.collection.AbstractIterator

def unfoldIterator[T](init: T)(next: T => Option[T]): Iterator[T] =
  Iterator(init) ++ Iterator.unfold(init)(next(_).map(x => (x, x)))

extension [A](it: Iterator[A])

  def at(i: Int): A = it.drop(i).next()

  def takeUntil(last: A => Boolean): Iterator[A] = new Iterator[A]:
    private var terminated = false

    override def hasNext: Boolean = it.hasNext && !terminated

    override def next(): A =
      if terminated then throw NoSuchElementException()
      val t = it.next()
      terminated = last(t)
      t

  def last: Option[A] =
    var t = it.nextOption()
    while it.hasNext do t = it.nextOption()
    t

  def zipWithTail: Iterator[(A, A)] =
    if it.hasNext
    then
      new AbstractIterator[(A, A)]:
        private var prev: A = it.next()
        override def hasNext: Boolean = it.hasNext
        override def next(): (A, A) =
          val cur = it.next()
          val ret = (prev, cur)
          prev = cur
          ret
    else
      Iterator.empty

  /*def zipTail: Iterator[(A, A)] = {
    it.sliding(2).map({ case Seq(a, b) => (a, b) }) // simpler but slower (by ~3s)
  }*/

  def zipWithPrev: Iterator[(Option[A], A)] =
    new AbstractIterator[(Option[A], A)]:
      private var prevOption: Option[A] = None
      override def hasNext: Boolean = it.hasNext
      override def next(): (Option[A], A) =
        val cur = it.next()
        val ret = (prevOption, cur)
        prevOption = Some(cur)
        ret

  def groupBy[K](key: A => K): Map[K, Iterator[A]] =
    it.to(LazyList).groupBy(key).view.mapValues(_.iterator).toMap

  def groupMap[K, B](key: A => K)(f: A => B): Map[K, Iterator[B]] =
    it.to(LazyList).groupMap(key)(f).view.mapValues(_.iterator).toMap

  def groupMapReduce[K, B](key: A => K)(f: A => B)(reduce: (B, B) => B): Map[K, B] =
    it.to(LazyList).groupMapReduce(key)(f)(reduce)

  def groupCount[K](key: A => K): Map[K, Int] =  it.groupMapReduce(key)(_ => 1)(_ + _)
