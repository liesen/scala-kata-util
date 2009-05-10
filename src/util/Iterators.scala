package util

object Iterators {
  def iterate[A](seed: A, step: A => A): Iterator[A] = new Iterator[A] {
    private var element = seed
    
    val hasNext: Boolean = true
    
    def next(): A = {
      val current = element
      element = step(element)
      current
    }
  }
}
