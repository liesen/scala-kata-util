package kata.primes
import java.io.{InputStream, FileInputStream}

class IndexedPrimesFile {
  val index = 0

  var position = 0

  // def drop(n: Int) = position += n
}

object PrimesFile {
  val PATH = "/Users/liesen/code/scala-kata-util/primes/millions/primes.dat"
  
  val INDEX = "/Users/liesen/code/scala-kata-util/primes/millions/primes.ix"

  private def withFile[A](thunk: FileInputStream => A): A = {
    val fin = new FileInputStream(PATH)
    val result = thunk(fin)
    fin.close
    result
  }

  def withPrimes[A](thunk: Iterator[Long] => A): A = withFile { fin =>
    thunk(new Iterator[Long] {
      def hasNext = fin.available > 0
      def next = decodeVarint64(fin)
    })
  }

  // def withPrimesIndex[A](thunk: Primes => A): A = {}

  private def decodeVarint64(in: InputStream): Long = {
    var shift = 0
    var result = 0L

    while (shift < 64) {
      val b = in.read & 0xff
      result |= (b & 0x7f) << shift
      if ((b & 0x80) == 0) return result
      shift += 7
    }

    error("malformed varint")
  }

  def createIndex(blockStep: Int): Map[Long, Long] = {
    Map.empty
  }
}
