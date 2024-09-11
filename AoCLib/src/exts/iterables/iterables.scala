package exts.iterables

import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.*

extension [A](it: IterableOnce[A])

  def countItems: Map[A, Int] =
    it.iterator.foldLeft(Map.empty[A, Int] withDefaultValue 0):
      (acc, p) => acc + (p -> (acc(p) + 1))

  def findFirst: Option[A] =
    val i = it.iterator
    if i.hasNext then Some(i.next()) else None

extension [A](it: Iterable[A])

  def findFirstAsync[B](f: A => IterableOnce[B]): Option[B] =
    val promise = Promise[Option[B]]
    val left = new AtomicInteger(it.size)
    for a <- it do Future:
      findFirst(f(a).iterator).foreach(b => promise.trySuccess(Some(b)))
      if left.decrementAndGet() == 0 then promise.trySuccess(None)
    Await.result(promise.future, Duration.Inf)

  // https://stackoverflow.com/a/2099896
  def cycle: Iterator[A] = Iterator.continually(it).flatten

  def groupCount[K](key: A => K): Map[K, Int] = it.groupMapReduce(key)(_ => 1)(_ + _)

  def second: A = it.tail.head
  def first: A = it.head
