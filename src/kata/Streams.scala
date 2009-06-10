package kata

object Streams {
  /**
   * Creates an infinitely repeating stream of elements from /ss/.
   */
  def cycle[A](ss: Stream[A]): Stream[A] = ss lazy_::: cycle(ss)
  
  /**
   * Weaves a stream of elements from /ss/ by taking the head element from each
   * concatenated with their woven tails.
   * 
   * Drops streams that run dry of elements.
   * 
   * Example: 
   *   weave([1, 2, 3, 4, 5], [a, b, c], [!, #, %, &]) = ...
   *     [1, a, !, 2, b, #, 4, c, %, 4, &, 5]
   */
  def weave[A](streams: Stream[A]*): Stream[A] = {
    val nonEmpty = streams filter { !_.isEmpty }
    Stream(nonEmpty map { _.head }: _*) lazy_::: weave(nonEmpty map { _.tail }: _*)
  }
  
  def iterate[A](seed: A, step: A => A): Stream[A] = seed lazy_:: iterate(step(seed), step) 
}
