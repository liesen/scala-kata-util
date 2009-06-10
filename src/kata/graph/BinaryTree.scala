package graph


class BinaryTree[A](implicit view: A => Ordered[A]) extends Iterable[A] {
  abstract class Node[A] extends Iterable[A]

  sealed case class Branch[A](element: A, left: Unit => Node[A], right: Unit => Node[A]) extends Node[A] {
    def elements(): Iterator[A] = inorder
  
    def inorder(): Iterator[A] = Iterator.single(element) ++ left().elements ++ right().elements
  
    def preorder(): Iterator[A] = left().elements ++ Iterator.single(element) ++ right().elements
  
    def postorder(): Iterator[A] = right().elements ++ Iterator.single(element) ++ left().elements
  }

  final case object Empty extends Node[A] {
    lazy val elements = Nil.elements
  }
  
  implicit def toOrdered(value: A): Ordered[A] = view(value)
  
  
  val root: Node[A] = Empty
  
  def elements(): Iterator[A] = root.elements
  
  def contains(key: A): Boolean = {
    def containsAt(key: A, node: Node[A]): Boolean = {
      node match {
        case Branch(a, left, right) => 
          if (key == a) 
            true 
          else if (key < a) 
            containsAt(key, left()) 
          else 
            containsAt(key, right())
        case Empty => false
      }
    }
    
    containsAt(key, root)
  }
}
