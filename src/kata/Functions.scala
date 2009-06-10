package kata

object Functions {
  // Decorates a function, f :: b -> b -> c with a function, on :: a -> b,
  // that morphs f to have type: a -> a -> c
  implicit def withOn[B, C](f: (B, B) => C) = new {
    def on[A](g: A => B)(x: A, y: A): C = f(g(x), g(y))
  }
  
  // Tiny example
  def main(args: Array[String]): Unit = {    
    val lt: (Int, Int) => Boolean = _ < _
    val gt: (Int, Int) => Boolean = _ > _
    
    val as = List((2, 4), (3, 6), (1, 5))
    
    println(as)    
    println(as sort { lt on { _._1 } })
    println(as sort { gt on { _._1 } })
    
    assert((as sort { lt on { _._2 } }).reverse == (as sort { gt on { _._2 } }))
  }
}
