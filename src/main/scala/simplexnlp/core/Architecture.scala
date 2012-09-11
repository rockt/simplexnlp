package simplexnlp.core

import simplexnlp.core.Util._
import collection.mutable.ListBuffer

private class Architecture //just to stop IntelliJ from complaining FIXME: find a better solution

//TODO: class Workflow
//TODO: def |(that:Workflow)

//a chain of NLP components
class Pipeline(val components: Component*) {
  def ->(that: Pipeline) = new Pipeline((this.components ++ that.components): _*)
  def process(doc: Document) = {
    components.foreach((c: Component) => {
      c.preHook()
      c.process(doc)
      c.postHook()
    })
  }
  override def toString = components.map(getClassName(_)).mkString("*Input* => ", " -> ", " => *Output*")
  def initialize() {
    components.foreach(_.initialize)
  }
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
  //TODO: we need a flag that distinguishes between predictions and goldstandard
  //get the document (root ancestor)
  def doc: Document = {
    if (parent.isInstanceOf[Document]) parent.asInstanceOf[Document]
    //recursion
    else parent.asInstanceOf[Annotation].doc
  }
}

//a document with a text and annotations
class Document(id: String, val text: String) extends Annotation with ParentOf[Annotation] {
  override def doc = this
  def sentences = childrenFilteredBy[Sentence]
}

trait Parameters {
  import scala.collection.mutable.HashMap
  private val params = new HashMap[String, AnyVal]
  def parameters(tuple: Tuple2[String, _]*): Unit = {
    tuple.foreach((t: Tuple2[String, _]) => params.put(t._1, t._2.asInstanceOf[AnyVal]))
  }
  def parameters(key:String) = params(key)
}

//TODO: implement Input und Output type specification
//a NLP component
abstract class Component {
  //a concrete component needs to override this method
  def process(doc: Document)
  def initialize() {
    //something to do before start processing (e.g. loading parameters)
  }
  def preHook() {
    //something to do before each call of process
  }
  def postHook() {
    //something to do after each call of process
  }
}

trait Span extends Annotation {
  val start: Int
  val end: Int
  assert(end - start >= 0, "A span must start before it ends!")
  //TODO: def append
  //TODO: def prepend
  //TODO: def trimStart
  //TODO: def trimEnd
  def enclosingText:String = {
    parent match {
      case span:Span => span.text
      case doc:Document => doc.text
    }
  }
  def startInDoc = {
    parent match {
      case span:Span => span.start + start
      case doc:Document => start
    }
  }
  def endInDoc = {
    parent match {
      case span:Span => span.start + end
      case doc:Document => end
    }
  }
  def text = doc.text.substring(startInDoc, endInDoc+1)
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
  def relations = childrenFilteredBy[Relation]
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
