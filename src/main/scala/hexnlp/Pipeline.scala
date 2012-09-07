package hexnlp


import collection.mutable.ListBuffer
import collection.mutable.HashSet
import edu.uchsc.ccp.nlp.ei.mutation.MutationFinder

//TODO: annotation should be connected to a document!
trait Annotation {
  var doc:Document = _
  def add(doc:Document) = doc.add(this)
  def remove = doc.remove(this)
}

class Document(val text:String) {
  var annotations = new HashSet[Annotation]
  def add(a:Annotation) = annotations.add(a)
  def +(a:Annotation) = annotations.add(a)
  def remove(a:Annotation) = annotations.remove(a)
  def -(a:Annotation) = annotations.remove(a)
  override def toString = annotations.toString()
}

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
  assert(end - start >= 0, "Span must end after its beginning!")
  //TODO: def doc = doc.doc.substring(start, end+1)
  //TODO: def append
  //TODO: def prepend
  //TODO: def trimStart
  //TODO: def trimEnd
}

trait NonOverlappingSpan extends Span with Ordered[NonOverlappingSpan] {
  override def compare(that:NonOverlappingSpan):Int = {
    assert((this.start < that.start || this.start > that.start) && (this.end <= that.start || this.start >= that.end), this + " overlaps " + that)
    this.start - that.start
  }
}

//TODO: sentence should contain a list of tokens
case class Sentence(start:Int, end:Int) extends NonOverlappingSpan
case class Token(start:Int, end:Int) extends NonOverlappingSpan

trait Entity extends Span
abstract class Relation(entities:Entity*) extends Annotation


//Example NLP pipeline
case class Mutation(start:Int, end:Int) extends Entity
case class Gene(start:Int, end:Int) extends Entity
case class Phenotype(start:Int, end:Int) extends Entity
case class Drug(start:Int, end:Int) extends Entity

case class PPI(a:Gene, b:Gene) extends Relation(a, b) {
  override def toString:String = "PPI: " + a + " - " + b
}
case class DDI(a:Drug, b:Drug) extends Relation(a, b) {
  override def toString:String = "DDI: " + a + " - " + b
}
case class GPI(a:Gene, b:Phenotype) extends Relation(a, b) {
  override def toString:String = "GPI: " + a + " - " + b
}


class DummyGeneAnnotator extends Component {
  override def process(doc: Document) = doc + new Gene(0,0)
}

class DummyDiseaseAnnotator extends Component {
  override def initialize() { println("Phenotype tagger initialized.") }
  override def preHook() { println("Start tagging disease...") }
  override def process(doc: Document) = doc + new Phenotype(2,3)
  override def postHook() { println("End tagging disease.") }
}

class MutationAnnotator extends Component {
  var extractor:MutationFinder  = _
  override def initialize() {
    extractor = new MutationFinder("./resources/mutationFinder/regex.txt")
  }

  override def process(doc:Document) = {
    import scala.collection.JavaConversions._
    val mutations = extractor.extractMutations(doc.text)
    for (mutation <- mutations.keySet(); span <- mutations.get(mutation)) {
      println((span.asInstanceOf[Array[Int]](0),span.asInstanceOf[Array[Int]](1)))
    }
  }
}

object Prototype extends App {
  implicit def componentToPipeline(component:Component):Pipeline = new Pipeline(component) //TODO: find a better place for this conversion
  val c1 = new DummyGeneAnnotator
  val c2 = new DummyDiseaseAnnotator
  val m = new MutationAnnotator
  val doc = new Document("This disease is caused by the A54T substitution in gene XYZ.")
  val pipeline:Pipeline = c1 ++ c2 ++ c1 ++ m
  val result = pipeline.process(doc)
  val relation = PPI(Gene(1,2), Gene(2,3))
  println(pipeline)
  println(doc.text)
  println(result)
  println(relation)
  val s1 = new Sentence(0,3)
  val s2 = new Sentence(3,4)
  val s3 = new Sentence(0,3)
  println(s1 < s2)
  println(s2 <= s1)
  result.foreach((a:Annotation) =>
    a match {
      //case s:Span => println(s.doc)
      case _ => //
    }
  )
}