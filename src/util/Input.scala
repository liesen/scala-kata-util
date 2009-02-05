package util

object Input {
  import java.util.Scanner
  import java.io.File
  import java.io._
  
  def newScanner(file: String): Scanner = new Scanner(new File(file))
  
  implicit def scanner2iterator(sc: Scanner): Iterator[String] = new Iterator[String] {
    def hasNext(): Boolean = sc.hasNext
    
    def next(): String = sc.next
  }
  
  def lines(path: String): Iterator[String] = new Iterator[String] {
    val file = new File(path)
    val reader = new BufferedReader(new FileReader(file))
    var line: String = null
   
    def hasNext(): Boolean = { 
      line = reader.readLine
      line != null
    }
    
    def next(): String = line
  }
  
  
  def split(it: Iterator[String], delimiter: String): Iterator[String] = 
    if (!it.hasNext)
      Iterator.empty
    else new Iterator[String] {
      var sc = new Scanner(it.next).useDelimiter(delimiter)
      
      def hasNext(): Boolean = 
        if (sc.hasNext)
          true
        else if (it.hasNext) {
          sc = new Scanner(it.next).useDelimiter(delimiter)
          sc.hasNext
        } else
          false
        
      def next(): String = sc.next
    }
}
