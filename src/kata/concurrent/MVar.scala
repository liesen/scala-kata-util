package kata.concurrent

import java.util.concurrent._
import java.util.concurrent.locks._

class MVar[A] {
  val lock = new ReentrantReadWriteLock
  
  var value: A = _
  
  def take(): A = {
    lock.readLock.lock
    
    try {
      return value
    } finally {
      lock.readLock.unlock
    }
  }
  
  def isEmpty = value != null
  
  // with
  def flatMap[B](thunk: A => B): B = {
    val a = read
    
    try {
      thunk(a)
    } catch {
      case e => put(a); throw e
    }
  }
  
  def foreach(f: A => Unit): Unit = f(read)
  
  def put(value: A): Unit = {
    lock.writeLock.lock
    
    try {
      this.value = value 
    } finally {
      lock.writeLock.unlock
    }
  }
  
  def read(): A = {
    lock.readLock.lock
    
    try {
      val v = take
      put(v)
      v
    } finally {
      lock.readLock.unlock
    }
  }
  
  def swap(newValue: A): A = {
    val oldValue = take
    put(newValue)
    oldValue
  }
}

object MVar {
  def apply[A](value: A): MVar[A] = {
    val mvar = new MVar[A]
    mvar put value
    mvar
  }
  
  def main(args : Array[String]) : Unit = {
    println("Testing")
    val m = new MVar[String]
    
    val t = new Thread {
      override def run = {
        println("Started thread")
        try {
          Thread sleep 3000
          m put "Done"
        } catch {
          case _ => ()
        }
      }
    }
    
    t.start
    t.join
    
    for (value <- m) {
      println(value)
    }
  }
}
