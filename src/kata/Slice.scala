package kata

/** A Slice is a range over some domain. */
class Slice(val f: Int => Long) {
  var lo = 0
  var hi = 0
  var sum = 0L
  
  def reset {
    lo = 0
    hi = 0
    sum = 0L
  }
  
  def shrink {
    sum -= f(lo)
    lo += 1
    assert(lo < hi, "there are rules!")
  }
  
  def grow {
    sum += f(hi)
    hi += 1
    assert(lo < hi, "growing pains!")
  }
  
  def slide {
    grow
    shrink
  }
  
  def size = hi - lo
  
  override def toString = "[" + lo + ", " + hi + ") = " + sum
}
