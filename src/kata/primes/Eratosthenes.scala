package kata.primes

// Rip-off from Sebastian Fischer's http://hackage.haskell.org/cgi-bin/hackage-scripts/package/primes

object Eratosthenes {
  type N = Long
  
  type Wheel = (Stream[N], Stream[N])
  
  abstract class Queue {
    def enqueue(ns: Stream[N]): Queue = merge(Fork(ns, Stream.empty))
    
    def merge(queue: Queue): Queue
    
    def dequeue(): (Stream[N], Queue)
  }
  
  object Empty extends Queue {
    def merge(queue: Queue): Queue = queue
    
    def dequeue(): (Stream[N], Queue) = (Stream.empty, Empty)
  }
  
  case class Fork(ns: Stream[N], queues: Stream[Queue]) extends Queue {
    def priority = ns.head
    
    def merge(queue: Queue): Queue = queue match {
      case Empty          => this
      case q @ Fork(_, _) => if (priority <= q.priority) this join q else q join this
    } 
      
    def join(queue: Queue): Queue = Fork(ns, queue lazy_:: queues)
    
    def dequeue(): (Stream[N], Queue) = (ns, Queue.mergeAll(queues))
  }
  
  object Queue {
    def empty: Queue = Empty
    
    def mergeAll(queues: Iterable[Queue]): Queue = queues.foldLeft(empty){ _ merge _ }
  }

  def wheelSieve(k: Int): Stream[N] = {
    def spin(n: N)(xss: Stream[N]): Stream[N] = xss match {
      case Stream.cons(x, xs) => n lazy_:: spin(n + x)(xs)
    }
    
    val ((Stream.cons(p, ps)), ns) = wheel(k)
    
    ps.reverse lazy_::: sieve(spin(p)(cycle(ns)), Empty)
  }
  
  def sieve(nss: Stream[N], queue: Queue): Stream[N] = nss match {
    case Stream.cons(n, ns) => queue match {
      case Empty => sieve(nss, Empty enqueue ((n lazy_:: ns) map { _ * n }))
      case _     => {
        val (Stream.cons(m, ms), q) = queue.dequeue
      
        if (m == n)
          sieve(ns, q enqueue ms)
        else if (m < n)
          sieve(n lazy_:: ns, q enqueue ms)
        else
          n lazy_:: sieve(ns, queue enqueue ((n lazy_:: ns) map { _ * n }))
      }
    }
  }
  
  def cycle[A](xs: Stream[A]): Stream[A] = xs lazy_::: cycle(xs)
  
  def iterate[A](seed: A, step: A => A): Stream[A] = seed lazy_:: iterate(step(seed), step)
  
  def wheel(k: Int): Wheel = (iterate((Stream(2L), Stream(1L)), next _) drop (k - 1)).head
  
  def next(wheel: Wheel): Wheel = wheel match {
    case (ps @ Stream.cons(p, _), xs) => {
      val Stream.cons(y, ys) = cycle(xs)
      val py = p + y
      (py lazy_:: ps, cancel(ps reduceLeft { _ * _ }, p, py, ys))
    }
  }
  
  def cancel(m: N, p: N, n: N, xs: Stream[N]): Stream[N] = m match {
    case 0 => Stream.empty
    case _ => xs match {
      case Stream.cons(x, ys @ Stream.cons(y, zs)) => {
        val nx = n + x
        
        if (nx % p > 0)
          x lazy_:: cancel(m - x, p, nx, ys)
        else
          cancel(m, p, n, (x + y) lazy_:: zs)
      } 
    }
  }
  
  def main(args: Array[String]): Unit = {
    val primes = Primes.primes
    
    wheelSieve(6).zipWithIndex find { case (prime, i) => println(prime); prime != primes(i) } match {
      case Some((prime, i)) => println("at", i, " -> ", prime, "is not eq to", primes(i))
      case _                => println("whaoo")
    }
  }
}
