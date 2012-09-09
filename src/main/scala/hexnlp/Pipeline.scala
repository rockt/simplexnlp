package hexnlp

import collection.mutable.ListBuffer
import edu.uchsc.ccp.nlp.ei.mutation.MutationFinder
import hexnlp.Util._
import hexnlp.Implicits._

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

//TODO: implement a Parameter class for components
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

//example NLP pipeline
case class Mutation(start: Int, end: Int) extends Entity
case class Gene(start: Int, end: Int) extends Entity
case class Disease(start: Int, end: Int) extends Entity
case class Drug(start: Int, end: Int) extends Entity

case class PPI(a: Gene, b: Gene) extends Relation(a, b) {
  override def toString: String = "PPI: " + a + " - " + b
}
case class DDI(a: Drug, b: Drug) extends Relation(a, b) {
  override def toString: String = "DDI: " + a + " - " + b
}
case class MutationDiseaseRelation(m: Mutation, d: Disease) extends Relation(m, d) {
  override def toString: String = "MutationDiseaseRelation: " + m + " - " + d
}

class DummyDiseaseAnnotator extends Component {
  val DISEASE = "disease"
  override def process(doc: Document) = {
    val result = doc.text.indexOf(DISEASE)
    if (result >= 0) doc + Disease(result, result + DISEASE.length)
  }
}

class DummySentenceAnnotator extends Component {
  override def process(doc: Document) = {
    doc + Sentence(0, doc.text.length)
  }
}

class MutationAnnotator extends Component {
  var extractor: MutationFinder = _
  override def initialize() {
    suppressConsoleOutput {
      extractor = new MutationFinder("./resources/mutationFinder/regex.txt")
    }
  }
  override def process(doc: Document) = {
    import scala.collection.JavaConversions._
    for (sentence <- doc.sentences) {
      val mutations = extractor.extractMutations(sentence.text)
      for (mutation <- mutations.keySet(); tuple <- mutations.get(mutation)) {
        val span = tuple.asInstanceOf[Array[Int]]
        sentence + Mutation(span(0), span(1))
      }
    }
  }
}

class CoOccurrenceAnnotator extends Component {
  override def process(doc: Document) = {
    //TODO
  }
}

object Prototype extends App {
  val d = new DummyDiseaseAnnotator
  val s = new DummySentenceAnnotator
  val m = new MutationAnnotator
  val doc = new Document("This disease is caused by the A54T substitution in gene XYZ.")
  val pipeline = s ++ d ++ m ++ d
  pipeline.process(doc)
  println("Pipeline:\t" + pipeline)
  println("Text:\t\t" + doc.text)
  println("Sentences:")
  println(doc.sentences)
  println("Sentence descendants")
  for (sentence <- doc.sentences) {
    println(sentence.descendants)
  }
  println("All Annotations:")
  println(doc.descendants)
}