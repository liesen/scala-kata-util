package kata.ordering

/**
 * Maps a function on an object to provide an ordering. Useful for ordering objects based on a
 * certain property.
 * 
 * Usage:
 * 
 *   val xs = Array("bob", "jack", "ricardo", "steve")
 *   scala.util.Sorting.quickSort(xs)(compareOn(_.length)) // bob, jack, rufus, ricardo
 * 
 * Especially useful together with PriorityQueue:
 * 
 *   new scala.collection.mutable.PriorityQueue()(compareOn(_.someProperty))
 */
object Ordering {
  /**
   * Creates an <i>ordering</i> on a type {@code A} by applying {@code f} to 
   * both parameters of {@code compare}, thus creating a <i>view</i> on the 
   * parameters of types {@code B} (which is bounded by a total order view).
   */
  def orderingOn[A, B <% Ordered[B]](f: A => B): Ordering[A] = new Ordering[A] { 
    def compare(x: A, y: A): Int = f(x) compare f(y)
  }
  
  /**
   * Transformes a type, {@code A}, to a totally ordered type, {@code Ordered[A]}, given a 
   * function from {@code A} to {@code B} where {@code B} is <i>bounded by an ordering on</i>
   * {@code B}.
   * 
   * @param f a function applied on {@code x} that transformes {@code x} to a totally ordered type {@code B}.
   */
  def compareOn[A, B <% Ordered[B]](f: A => B)(x: A): Ordered[A] = new Ordered[A] {
    def compare(y: A): Int = orderingOn(f).compare(x, y)
  }
}
