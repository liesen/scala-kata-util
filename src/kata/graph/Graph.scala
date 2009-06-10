package kata.graph

import scala.collection.mutable.ListBuffer

/**
 * A directed graph whose elements are of type {@code A}.
 */
class DirectedGraph[A] {
  /**
   * Vertex
   */
  case class Vertex(val element: A) {    
    def connect(vertex: Vertex): Edge = {
      val e = makeEdge(this, vertex)
      edges = edges + e
      e
    }
    
    /** Incoming edges to this vertex. */
    def incoming(): Iterable[Edge] = edges filter { _.to equals this }
    
    /** Outgoing edges. */
    def outgoing(): Iterable[Edge] = edges filter { _.from equals this }
    
    override def equals(o: Any): Boolean = o.isInstanceOf[Vertex] && (o.asInstanceOf[Vertex].element equals element)
    
    override val hashCode: Int = element.hashCode // Defer hashCode call for faster lookups
    
    override def toString(): String = element.toString
  }
  
  /** 
   * Edge 
   */
  case class Edge(from: Vertex, to: Vertex) extends Pair[Vertex, Vertex](from, to)
  
  implicit def pair2edge(p: (A, A)): Edge = makeEdge(makeVertex(p._1), makeVertex(p._2))
  
  var vertices: Set[Vertex] = Set()
  
  var edges: Set[Edge] = Set()
  
  protected def makeEdge(from: Vertex, to: Vertex): Edge = Edge(from, to)
  
  protected def makeVertex(elem: A): Vertex = Vertex(elem)
  
  def get(elem: A): Option[Vertex] = vertices find { v => elem == v.element }
  
  def getOrElse(elem: A, thunk: => Vertex): Vertex = get(elem) match {
    case None         => thunk
    case Some(vertex) => vertex 
  }
  
  def contains(elem: A): Boolean = get(elem).isDefined
  
  def find(elem: A): Option[Vertex] = vertices find { _.element equals elem }
  
  def isEmpty(): Boolean = vertices.isEmpty
  
  def add(elem: A): Vertex = {
    val v = makeVertex(elem)
    vertices = vertices + v
    v
  }
  
  def remove(elem: A): Unit = {
    edges = edges filter { e => !(e.to equals elem) && !(e.from equals elem) }
    vertices = vertices filter { v => !(v equals elem) }
  }
  
  def remove(edge: Edge): Unit = {
    edges = edges filter { e => !(e equals edge) }
  }
  
  def remove(edge: (A, A)): Unit = remove(pair2edge(edge))
  
  def remove(vertex: Vertex): Unit = {
    vertex.outgoing foreach remove
    vertex.incoming foreach remove
    vertices filter { v => !(vertex equals v) }
  }
  
  def dijstra(source: A, end: Option[A])(implicit view: A => Long): Iterable[A] = {
    Nil
  }
  
  def dijkstra(source: Vertex, destination: Option[Vertex])(implicit view: A => Long): Iterable[A] = {
    import scala.collection.mutable.{ Set, Map, PriorityQueue }
    import kata.ordering.Ordering._
    
    var distances: Map[Vertex, Long] = Map()
      var Q: PriorityQueue[Vertex] = new PriorityQueue[Vertex]()(compareOn(view.compose(_.element)))
      var settled: Set[Vertex] = Set()
      var predecessors: Map[Vertex, Vertex] = Map()
      
      distances(source) = 0
      Q += source
      
      var reachedEnd = false
      
      while (!Q.isEmpty && !reachedEnd) {
        val u = Q.dequeue
        
        settled += u
        
        reachedEnd = destination.isDefined && u == destination.get
        
        if (!reachedEnd) {
          for (v <- u.outgoing map { _.to } if !settled.contains(v)) {
            val d = distances(u) + view(v.element)
            
            if (!distances.isDefinedAt(v) || d < distances(v)) {
              distances += v -> d
              predecessors += v -> u
              Q += v
            }
          }
        }
      }
      
      if (!reachedEnd) {
        Nil
      } else {
        var path = List(source)
        var vertex = destination.get
        
        while (vertex != source) {
          path ::= vertex
          vertex = predecessors(vertex)
        }
        
        path map { _.element }
      }
    }
  
  override def toString = (vertices.toList map { v => 
    "(" + v + " -> " + ((v.outgoing map { _._2 }) mkString ", ") + ")" 
  }) mkString ", " 
}

trait Weighted[W] {
  self: DirectedGraph[_] =>
  
  type Edge = WeightedEdge
  
  case class WeightedEdge(from: Vertex, to: Vertex, weight: W)
  
  def makeEdge(from: Vertex, to: Vertex, weight: W): WeightedEdge = WeightedEdge(from, to, weight)
}

class CycleException(msg: String) extends Exception(msg)

object Graph {
  def topologicalSort[A](g: DirectedGraph[A]): Iterable[A] = {
    var L: ListBuffer[g.Vertex] = new ListBuffer()
    var Q: ListBuffer[g.Vertex] = new ListBuffer()
    var n: g.Vertex = null
    
    Q ++=  g.vertices filter (_.incoming.isEmpty)
    
    while (!Q.isEmpty) {
      n = Q.first; Q drop 1
      L += n
      
      for (m <- n.outgoing) {
        g remove m
        
        if (m.to.incoming.isEmpty) {
          Q += m.to
        }
      }
    }
    
    if (g.edges.size > 0) {
      throw new CycleException("Graph contains a cycle somewhere within " +
        "these vertices: " + ((g.vertices -- L) mkString ", ") + 
          ". Last investigated vertex: " + n)
    }
    
    L map { _.element }
  }
  
  def completeDirected[A](vs: A*): DirectedGraph[A] = {
    val graph = new DirectedGraph[A]
    val vertices = vs map graph.add
    
    for (v <- vertices; u <- vertices if v != u) {
      v connect u
    }
    
    return graph
  }
}