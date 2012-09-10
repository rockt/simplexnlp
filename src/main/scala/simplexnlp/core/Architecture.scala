package simplexnlp.core

import simplexnlp.Util._
import collection.mutable.ListBuffer

private class Architecture //just to stop IntelliJ complaining FIXME: find a better solution

//a chain of NLP components
class Pipeline(val components: Component*) {
  def ++(that: Pipeline) = new Pipeline((this.components ++ that.components): _*)
  def process(doc: Document) = {
    components.foreach((c: Component) => {
      c.preHook()
      c.process(doc)
      c.postHook()
    })
  }
  override def toString = components.map(getClassName(_)).mkString("*Input* => ", " -> ", " => *Output*")
}

//something that has children
trait ParentOf[C <: Child] {
  private val childrenBuffer = new ListBuffer[C]
  def children = childrenBuffer.toList
  def add(child: C) = {
    childrenBuffer += child
    child.parent = this
  }
  def remove(child: C) = {
    childrenBuffer - child
    child.parent = null
  }
  def +(child: C) = add(child)
  def -(child: C) = remove(child)
  //gathers all descendants recursively
  def descendants: List[C] = {
    val buffer = new ListBuffer[C]
    for (child <- this.asInstanceOf[ParentOf[C]].childrenBuffer) {
      buffer.append(child)
      //recursion
      if (child.isInstanceOf[ParentOf[C]]) buffer.appendAll(child.asInstanceOf[ParentOf[C]].descendants)
    }
    buffer.toList
  }
  def childrenFilteredBy[T](implicit mf: Manifest[T]) = filterByType[T](children)
  def descendantsFilteredBy[T](implicit mf: Manifest[T]) = filterByType[T](descendants)
}

//something that has a parent
trait Child {
  var parent: Any = _
}

//an annotation refers to a document and might be nested in another annotation
trait Annotation extends Child {
  //get the document (root ancestor)
  def doc: Document = {
    if (parent.isInstanceOf[Document]) parent.asInstanceOf[Document]
    else parent.asInstanceOf[Annotation].doc
  }
}

//TODO: every document needs an ID
//a document with a text an annotations
class Document(val text: String) extends Annotation with ParentOf[Annotation] {
  override def doc = this
  def sentences = childrenFilteredBy[Sentence]
}

//TODO: implement a Parameter class (Map String -> Any) for components
//TODO: implement Input und Output type specification
//a NLP component
abstract class Component {
  def initialize() {}
  //a concrete component needs to override this method
  def process(doc: Document)
  def preHook() {}
  def postHook() {}
  initialize()
}

//TODO: class Workflow
//TODO: def |(that:Workflow)



trait Span extends Annotation {
  val start: Int
  val end: Int
  assert(end - start >= 0, "A span must start before it ends!")
  //TODO: def append
  //TODO: def prepend
  //TODO: def trimStart
  //TODO: def trimEnd
  def text = doc.text.substring(start, end)
  def length = text.length
  override def toString = getClassName(this) + "[" + start + "-" + end + "]: " + text
}

trait NonOverlappingSpan extends Span with Ordered[NonOverlappingSpan] {
  override def compare(that: NonOverlappingSpan): Int = {
    assert((this.start < that.start || this.start > that.start) && (this.end <= that.start || this.start >= that.end), this + " overlaps " + that)
    this.start - that.start
  }
}

case class Sentence(start: Int, end: Int) extends NonOverlappingSpan with ParentOf[Span] {
  def tokens = childrenFilteredBy[Token]
  def entities = childrenFilteredBy[Entity]
}

case class Token(start: Int, end: Int) extends NonOverlappingSpan {
  var pos: String = _
}

trait Entity extends Span

abstract class Relation(entities: Entity*) extends Span {
  //TODO: find start of first and end of last entity instead
  val start = entities.head.start
  val end = entities.head.end
  //TODO: a relation might have a trigger word
}

//TODO: implement nested relations
