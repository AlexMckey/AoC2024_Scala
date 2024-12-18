package parse

import scala.reflect.ClassTag
import scala.util.matching.Regex

private val NumRe: Regex = "-?\\d+".r

trait Read[A]:
  def read(input: String): A

  def map[B](f: A => B): Read[B] =
    val readA = read
    new Read[B]:
      def read(input: String): B =
        f(readA(input))

  def replaceAll(reps: List[(String,String)]): Read[A] =
    val readA = read
    new Read[A]:
      def read(input: String): A =
        readA(reps.foldLeft(input){ case (s, (from, to)) => s.replaceAll(from, to)})
    
given Read[Int] with
  def read(input: String): Int =
    input.toInt

given Read[Long] with
  def read(input: String): Long =
    input.toLong

given Read[BigInt] with
  def read(input: String): BigInt =
    BigInt(input)

given Read[String] with
  def read(input: String): String =
    input

given Read[Char] with
  def read(input: String): Char =
    if input.length == 1 then input.head
    else throw new Exception(s"Unable to parse $input into a Char")

//given Read[Json] with
//  def read(input: String): Json =
//    parse(input).getOrElse(throw new Exception(s"Unable to parse $input as Json"))

object Read:
  def product[T <: Product : ReadProduct](regex: Regex): Read[T] = new Read[T]:
    def read(input: String): T =
      regex.unapplySeq(input) match
        case Some(fields) => summon[ReadProduct[T]].readProduct(fields.toArray)
        case None => throw new Exception(s"Regex '${regex.regex}' did not match '$input'")

  def product[T <: Product : ReadProduct](delimiter: String): Read[T] = new Read[T]:
    def read(input: String): T =
      summon[ReadProduct[T]]
        .readProduct(input.split(delimiter))

  def seq[C[_] : ReadSeq, T: Read : ClassTag](delimiter: String): Read[C[T]] = new Read[C[T]]:
    def read(input: String): C[T] =
      summon[ReadSeq[C]]
        .readSeq[T](input.split(delimiter))

  def seq[C[_] : ReadSeq, T: Read : ClassTag](regex: Regex): Read[C[T]] = new Read[C[T]]:
    def read(input: String): C[T] =
      summon[ReadSeq[C]]
        .readSeq[T](regex.findAllMatchIn(input).map(_.matched).toArray)

  def ints[C[_] : ReadSeq]: Read[C[Int]] = new Read[C[Int]]:
    def read(input: String): C[Int] =
      summon[ReadSeq[C]]
        .readSeq[Int](NumRe.findAllMatchIn(input).map(_.matched).toArray)

  def longs[C[_] : ReadSeq]: Read[C[Long]] = new Read[C[Long]]:
    def read(input: String): C[Long] =
      summon[ReadSeq[C]]
        .readSeq[Long](NumRe.findAllMatchIn(input).map(_.matched).toArray)