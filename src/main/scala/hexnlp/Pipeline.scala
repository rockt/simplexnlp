package hexnlp

import collection.mutable.ListBuffer
import collection.mutable.HashSet
import edu.uchsc.ccp.nlp.ei.mutation.MutationFinder

trait Annotation {
  var doc:Document = _
  def add(doc:Document) = doc.add(this)
  def remove() = doc.remove(this)
}

// a mutable document with annotations
class Document(val text:String) {
  var annotations = new HashSet[Annotation]
  def add(a:Annotation) = this + a
  def +(a:Annotation) = {
    annotations.add(a)
    a.doc = this
  }
  def remove(a:Annotation) = this - a
  def -(a:Annotation) = {
    annotations.remove(a)
    a.doc = null
  }
  override def toString = annotations.toString()
}

//TODO: implement a Parameter class for components
abstract class Component {
  def initialize(){}
  def process(doc:Document) //a concrete component needs to override this method
  def preHook(){}
  def postHook(){}
  initialize()
}

class Pipeline(cs:Component*) {
  val components = new ListBuffer[Component]
  components.appendAll(cs)
  def ++(that:Pipeline) = new Pipeline((this.components ++ that.components): _*)
  //TODO: def |(that:Pipeline)
  def process(doc:Document) = {
    components.foreach((c:Component) => {
      c.preHook()
      c.process(doc)
      c.postHook()
      })
    doc.annotations
  }
  override def toString = components.toString()
}

trait Span extends Annotation {
  val start:Int
  val end:Int
  assert(end - start >= 0, "A span must end after its beginning!")
  //TODO: def append
  //TODO: def prepend
  //TODO: def trimStart
  //TODO: def trimEnd
  def text = doc.text.substring(start, end)
  def length = text.length
  override def toString = this.getClass.toString.substring(this.getClass.toString.lastIndexOf('.')+1) + "[" + start + "-" + end + "]:" + text
}

trait NonOverlappingSpan extends Span with Ordered[NonOverlappingSpan] {
  override def compare(that:NonOverlappingSpan):Int = {
    assert((this.start < that.start || this.start > that.start) && (this.end <= that.start || this.start >= that.end), this + " overlaps " + that)
    this.start - that.start
  }
}

trait ChildAnnotation extends Annotation {
  var parent:Annotation = _
  //TODO
}

//TODO: sentence should contain a list of tokens
case class Sentence(start:Int, end:Int) extends NonOverlappingSpan
case class Token(start:Int, end:Int) extends NonOverlappingSpan

trait Entity extends Span
abstract class Relation(entities:Entity*) extends Annotation

//Example NLP pipeline
case class Mutation(start:Int, end:Int) extends Entity
case class Gene(start:Int, end:Int) extends Entity
case class Disease(start:Int, end:Int) extends Entity
case class Drug(start:Int, end:Int) extends Entity

case class PPI(a:Gene, b:Gene) extends Relation(a, b) {
  override def toString:String = "PPI: " + a + " - " + b
}
case class DDI(a:Drug, b:Drug) extends Relation(a, b) {
  override def toString:String = "DDI: " + a + " - " + b
}
case class MutationDiseaseRelation(m:Mutation, d:Disease) extends Relation(m, d) {
  override def toString:String = "MutationDiseaseRelation: " + m + " - " + d
}

class DummyDiseaseAnnotator extends Component {
  val DISEASE = "disease"
  override def process(doc: Document) = {
    val result = doc.text.indexOf(DISEASE)
    if (result >= 0) doc + new Disease(result, result + DISEASE.length)
  }
}

class MutationAnnotator extends Component {
  var extractor:MutationFinder  = _
  override def initialize() {
    extractor = new MutationFinder("./resources/mutationFinder/regex.txt")
  }

  override def process(doc:Document) = {
    import scala.collection.JavaConversions._
    val mutations = extractor.extractMutations(doc.text)
    for (mutation <- mutations.keySet(); tuple <- mutations.get(mutation)) {
      val span = tuple.asInstanceOf[Array[Int]]
      doc + new Mutation(span(0), span(1))
    }
  }
}

class CoOccurrenceAnnotator extends Component {
  override def process(doc:Document) = {
    //TODO
  }
}

object Prototype extends App {
  implicit def componentToPipeline(component:Component):Pipeline = new Pipeline(component) //TODO: find a better place for this conversion
  val c1 = new DummyDiseaseAnnotator
  val c2 = new MutationAnnotator
  val doc = new Document("This disease is caused by the A54T substitution in gene XYZ.")
  val pipeline = c1 ++ c2 ++ c1
  val result = pipeline.process(doc)
  println("Pipeline:\t" + pipeline)
  println("Text:\t\t" + doc.text)
  println("Annotations:" + result)

  //TODO: only print Mutations
  result.foreach((a:Annotation) =>
    a match {
      //case s:Span => println(s.doc)
      case _ => //
    }
  )
}