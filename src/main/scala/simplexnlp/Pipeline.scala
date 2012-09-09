package simplexnlp

import collection.mutable.ListBuffer
import simplexnlp.Util._

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
  def childrenFilterBy[T](implicit mf: Manifest[T]) = filterByType[T](children)
  def descendantsFilterBy[T](implicit mf: Manifest[T]) = filterByType[T](descendants)
}

//something that has a parent
trait Child {
  var parent: Any = _
}

trait Annotation extends Child {
  //the parent of an annotation is a document
  def doc: Document = {
    if (parent.isInstanceOf[Document]) parent.asInstanceOf[Document]
    else parent.asInstanceOf[Annotation].doc
  }
}

//TODO: every document needs an ID
//a document with a text an annotations
class Document(val text: String) extends Annotation with ParentOf[Annotation] {
  override def doc = this
  def sentences = childrenFilterBy[Sentence]
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

//a chain of NLP components
class Pipeline(val components: Component*) {
  def ++(that: Pipeline) = new Pipeline((this.components ++ that.components): _*)
  //TODO: def |(that:Pipeline)
  def process(doc: Document) = {
    components.foreach((c: Component) => {
      c.preHook()
      c.process(doc)
      c.postHook()
    })
  }
  override def toString = components.map(getClassName(_)).mkString("*Input* => ", " -> ", " => *Output*")
}

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
  def tokens = childrenFilterBy[Token]
  def entities = childrenFilterBy[Entity]
  //FIXME: to specific here
  def genes = childrenFilterBy[Gene]
  def mutations = childrenFilterBy[Mutation]
  def diseases = childrenFilterBy[Disease]
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