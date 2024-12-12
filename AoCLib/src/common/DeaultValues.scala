package common

import scala.util.NotGiven

object DefaultValues:
  given booleanDefualts: Default[Boolean] = false
  given unitDefaults: Default[Unit] = ()
  given stringDefaults: Default[String] = ""
  given charDefaults: Default[Char] = '.'
  inline given [T](using num: Numeric[T]): Default[T] = num.zero
  given [T]: Default[Option[T]] = None
  given [T]: Default[Seq[T]] = Nil

object SubclassDefaults:
  given [T, U <: T](using defU: Default[U], _noDefaultForT: NotGiven[Default[T]]): Default[T] = defU.defaultValue
