package object common:

  //def default[A : Default]: A = summon[Default[A]].defaultValue
  
  def time[T](action: => T): (T, Long) =
    val start = System.nanoTime()
    val res   = action
    val time  = System.nanoTime() - start
    res -> time