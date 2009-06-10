package kata

class Matrix[A](val width: Int, val height: Int) extends PartialFunction[(Int, Int), A] with Collection[A] {
  // type Matrix = Array[Array[A]]
  
  trait Projection {
    def apply(w: Int, h: Int): A
    
    def update(w: Int, h: Int, value: A): Unit
    
    def dimension(): (Int, Int)
  }
  
  val matrix: Array[Array[A]] = new Array(height, width)
  
  val dimension: (Int, Int) = (width, height)
  
  private[this] val none = null.asInstanceOf[A]
  
  def apply(at: (Int, Int)): A = at match { case (w, h) => apply(w, h) }
  
  def apply(w: Int, h: Int): A = matrix(h)(w)
  
  def update(at: (Int, Int), value: A): Unit = at match { case (w, h) => update(w, h, value) }
  
  def update(w: Int, h: Int, value: A): Unit = matrix(h)(w) = value
  
  def get(w: Int, h: Int): Option[A] = if (isInsideBounds(w, h)) Some(matrix(h)(w)) else None
  
  def isDefinedAt(at: (Int, Int)): Boolean = at match { case (w, h) => isInsideBounds(w, h) }
  
  def isInsideBounds(w: Int, h: Int): Boolean = w >= 0 && w < width && h >= 0 && h < height
  
  def transpose(): Projection = new Projection {
    val dimension = (width, height)
    
    def apply(w: Int, h: Int) = matrix(h)(w)
    
    def update(w: Int, h: Int, value: A) = matrix(h)(w) = value
  }
  
  override def size(): Int = width * height
  
  def elements(): Iterator[A] = Iterator.fromArray(matrix(0))
  
  override def toString(): String = matrix.deepToString
}
