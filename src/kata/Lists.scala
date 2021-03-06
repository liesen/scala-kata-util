package kata

object Lists {
  import scala.collection.mutable.{ Buffer, ListBuffer }
  
  def permutations[A](xs: Iterable[A]): List[List[A]] =
    if (xs.isEmpty) {
      List(Nil)
    } else {
      for ((y, ys) <- selections(xs); ps <- permutations(ys)) yield y :: ps
    }

  def selections[A](xss: Iterable[A]): List[(A, List[A])] = xss match {
    case Nil => Nil
    case x :: xs => (x, xs) :: (for ((y, ys) <- selections(xs)) yield (y, x :: ys))
  }
  
  def sum(xs: Iterable[Long]): BigInt = (BigInt(0) /: xs) { _ + _ }
  
  def bigSum(xs: Iterable[BigInt]): BigInt = (BigInt(0) /: xs) { _ + _ }
  
  def product(xs: Iterable[Long]): BigInt = (BigInt(0) /: xs) { _ * _ }
  
  def groupBy[A](p: A => A => Boolean)(xss: List[A]): List[List[A]] = xss match {
    case Nil     => Nil
    case x :: xs => xs span p(x) match {
      case (ys, zs) => (x :: ys) :: groupBy(p)(zs)
    }
  }
  
  def group[A]: List[A] => List[List[A]] = groupBy(x => x == _)
  
  def count[A](xss: Iterable[List[A]]): Iterable[(A, Int)] = for (x <- xss) yield (x.head, x.size)
  
  def tails[A](xss: Seq[A]): Iterable[Seq[A]] = (0 to xss.size) map xss.drop
  
  def surround[A](x: A)(xs: Buffer[A]): Buffer[A] = (xs + x).+:(x)
  
  def palindromes[A](length: Int, alphabet: Set[A]): Iterator[Buffer[A]] =    
    alphabet.elements flatMap { digit => 
      if (length > 2)
        palindromes(length - 2, alphabet) map surround(digit)
      else if (length > 1)
        Iterator.single(surround(digit)(new ListBuffer))
      else
        Iterator.single(new ListBuffer + digit)
    }
}
