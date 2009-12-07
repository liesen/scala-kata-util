package kata

object Exec {
  import java.io.PrintStream
  import java.util.concurrent._ // { FutureTask, Callable, TimeUnit, TimeoutException }
  
  // Creates an instance of Callable for a function.
  implicit def toCallable[V](f: => V): Callable[V] = new Callable[V] {
    def call(): V = f
  }
  
  /**
   * Prints 'label: time' when /thunk/ has returned.
   */
  def printExecTime[A](out: PrintStream, label: String)(thunk: => A): A = withExecTime(thunk) match {
    case (result, time) => {
      out.printf("%s: %s%n", label, format(time))
      result
    }
  }
  
  /**
   */
  def printExecTime[A](out: PrintStream): (=> A) => A = printExecTime(out, "Execution time")
  
  /**
   * Returns the result of /f/ together with the execution time in milliseconds.
   */
  def withExecTime[A](f: => A): (A, Long) = {
    val start = System.currentTimeMillis
    val result = f
    (result, System.currentTimeMillis - start)
  }
  
  /**
   * 
   */
  def async[A, V](callback: V => A)(f: => V): Unit = {
    val executor = Executors.newSingleThreadExecutor
    executor.execute(new FutureTask(new Callable[A] {
      def call(): A = callback(f)
    }))
    executor.shutdown
  }
  
  /** 
   * Executes /f/ but terminates within (slightly after) /timeout/ ms.
   */
  def withTimeout[V](timeoutMs: Long)(f: => V): Option[V] = {    
    val task = new FutureTask(f)
    
    task.run // Sets the task in its run state
    
    try {
      Some(task.get(timeoutMs, TimeUnit.MILLISECONDS))
    } catch {
      case _ => None
    }
  }
  
  // Formats a time span in /milliseconds/ as hh:mm:ss (ms)
  def format(millis: Long): String = {
    import java.lang.{ Integer, Long }
    
    val s = millis / 1000
    
    val ms: Long = millis
    val ss: Integer = (s % 60).toInt
    val mm: Integer = ((s / 60) % 60).toInt
    val hh: Integer = (s / 3600).toInt
    
    String.format("%02dh %02dm %02ds (%dms)", hh, mm, ss, ms)
  }
}
