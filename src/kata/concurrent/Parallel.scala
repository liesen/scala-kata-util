package kata.concurrent

object Parallel {
  import java.util.concurrent._
  
  implicit val defaultExecutor = Executors.newFixedThreadPool(
    Runtime.getRuntime.availableProcessors)
  
  implicit def function2callable[A](f: => A): Callable[A] = new Callable[A] {
    def call = f
  }
  
  def par[T1, T2](f: => T1, g: => T2)(implicit executor: ExecutorService): T2 = {
    executor.submit(f).get
    executor.submit(g).get
  }
  
  
  def main(args : Array[String]) : Unit = {
    par(println("whatever"), println("something"))
    
  }
}