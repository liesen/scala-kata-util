package kata.primes

import java.nio.channels.ReadableByteChannel
import java.nio.ByteBuffer
import java.io.FileInputStream
import java.nio.channels.FileChannel

object BitFile {
  val path = "/Users/liesen/code/scala-kata-util/primes/millions/primes.bin"

  object Mode extends Enumeration {
    val VIEW, MEMORY, MMAP = Value
  }

  type Mode = Mode.Value

  def apply(mode: Mode): Primes = {
    if (mode == Mode.VIEW) viewFile
    else if (mode == Mode.MEMORY) readFile
    else mmapFile
  }

  def mmapFile = viewFile // TODO
  def readFile = viewFile

  def viewFile = new Primes {
    val chan = new FileInputStream(path).getChannel
    var position = 0L
    val bits = 0: Byte
    val buf = ByteBuffer.wrap(Array(bits))

    def isPrime(m: Long): Boolean = {
      println(m, m >> 3, position)

      //if ((m >> 3) != position) {
        position = m >> 3
        chan.read(buf, position)
      //}

      println("read ", bits, Integer.toBinaryString(bits & 0xff))

      (bits & (1 << (m & 3))) != 0
    }
  }
}