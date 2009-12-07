package kata.graphviz

import java.io.PrintWriter
import java.io.StringWriter
import java.io.Writer

class DirectedGraph {
  type V = DirectedGraph#Vertex[_]
  
  class Edge(val source: V, val destination: V) extends Pair[V, V](source, destination) {
    val connector: String = "--"

    var label: Option[String] = None

    def withLabel(text: String): Edge = { label = Some(text); this }

    override def toString = "\"%s\" %s \"%s\"".format(source, connector, destination)
  }
  
  trait Label {
    def label: String

    override def toString = super.toString + " [label = \"%s\"]".format(label)
  }

  class DirectedEdge(override val source: V, override val destination: V) extends Edge(source, destination) {
    override val connector = "->"
  }

  class Vertex[A](val element: A) {
    def -->[B](destination: Vertex[B]): B = {
      graph += apply(element) -> (graph.getOrElse(this, Set.empty) + destination)
      destination.element
    }

    def -->(destination: V): Unit =
      graph += this -> (graph.getOrElse(this, Set.empty) + destination)

    def -*->[B](destinations: Iterable[B]): Iterable[B] = {
      destinations foreach { destination => this --> vertices.getOrElseUpdate(destination, new Vertex(destination)) }
      destinations
    }

    def --[B](label: String) = new {
      def ->(destination: Vertex[B]): B = {
        edges += new DirectedEdge(Vertex.this, destination) withLabel label
        destination.element
      }
    }

    override def toString = element.toString
  }

  private var graph = Map.empty[V, Set[V]]

  private var edges = Set.empty[Edge]

  private val vertices = scala.collection.mutable.Map.empty[Any, V]

  private val subgraphs = scala.collection.mutable.Set.empty[DirectedGraph]

  // Construct a vertex
  def <<[A](v: A): V = apply(v)

  def apply[A](v: A): V = vertices.getOrElseUpdate(v, new Vertex(v))

  def subgraph = {
    val g = new DirectedGraph
    subgraphs += g
    g
  }

  def simplify: DirectedGraph = this

  override def toString(): String = write("graph", new StringWriter).toString

  def write[W <: Writer](graphType: String, out: W): W = {
    out write graphType
    out write " {\n"

    for (subgraph <- subgraphs) {
      subgraph.write("subgraph", out)
    }

    for { (source, destinations) <- graph
          destination <- destinations } {
      out write "  "
      out write new Edge(source, destination).toString
      out write ";\n"
    }

    out write "}\n"
    out
  }
}
