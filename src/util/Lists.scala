package util

object Lists {
  import scala.collection.mutable.{ Buffer, ListBuffer }
  
  def permutations[A](xss: List[A]): List[List[A]] = xss match {
    case Nil => List(Nil)
    case xs  => for ((y, ys) <- selections(xs); ps <- permutations(ys))
      yield y :: ps
  }

  def selections[A](xss: List[A]): List[(A, List[A])] = xss match {
    case Nil => Nil
    case x :: xs => (x, xs) :: (for ((y, ys) <- selections(xs)) yield (y, x :: ys))
  }
  
  def sum(xs: Iterable[Long]): BigInt = xs.foldLeft(BigInt(0)){ _ + _ }
  
  def bigSum(xs: Iterable[BigInt]): BigInt = xs.foldLeft(BigInt(0)){ _ + _ }
  
  def product(xs: Iterable[Long]): BigInt = xs.foldLeft(BigInt(0)){ _ * _ }
  
  def groupBy[A](p: A => A => Boolean)(xss: List[A]): List[List[A]] = xss match {
    case Nil     => Nil
    case x :: xs => xs span p(x) match {
      case (ys, zs) => (x :: ys) :: groupBy(p)(zs)
    }
  }
  
  def group[A](xss: List[A]): List[List[A]] = xss match {
    case Nil     => Nil
    case x :: xs => List(x :: xs takeWhile { _ == x }) ::: group(xs dropWhile { _ == x })
  }
  
  def count[A](xss: Iterable[List[A]]): Iterable[(A, Int)] = for (x <- xss) yield (x.head, x.size)
  
  def surround[A](x: A)(xs: Buffer[A]): Buffer[A] = (xs + x).+:(x)
  
  def palindromes[A](length: Int, alphabet: Set[A]): Iterator[Buffer[A]] = {    
    alphabet.elements flatMap { digit => 
      if (length > 2)
        palindromes(length - 2, alphabet) map surround(digit)
      else if (length > 1)
        Iterator.single(surround(digit)(new ListBuffer))
      else
        Iterator.single(new ListBuffer + digit)
    }
  }
}
