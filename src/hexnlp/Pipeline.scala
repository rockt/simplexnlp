package hexnlp


import collection.mutable.ListBuffer
import collection.mutable.HashSet

//TODO: annotation should be connected to a document!
trait Annotation {
  //var doc:Document = _ //TODO: change doc into a constructor parameter -> an annotation without a doc makes no sense!
  //def add(doc:Document) = doc.add(this)
  //def delete = doc.remove(this)

}

class Document(val text:String) {
  var annotations = new HashSet[Annotation]
  def add(a:Annotation) = annotations.add(a)
  def remove(a:Annotation) = annotations.remove(a)
  override def toString = annotations.toString()
}

abstract class Component {
  def initialize(){}
  def process(text:Document):Document //a concrete component needs to override this method
  def preHook(){}
  def postHook(){}
  initialize()
}

class Pipeline(cs:Component*) {
  val components = new ListBuffer[Component]
  components.appendAll(cs)
  def ++(that:Pipeline) = new Pipeline((this.components ++ that.components): _*)
  //TODO: def |(that:Pipeline)
  def process(text:Document):Document = {
    components.foreach((c:Component) => {
      c.preHook()
      c.process(text)
      c.postHook()
      })
    text
  }
  override def toString = components.toString()
}

trait Span extends Annotation {
  val start:Int
  val end:Int
  assert(end - start >= 0, "Span must end after its beginning!")
  //TODO: def text = doc.text.substring(start, end+1)
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
  override def process(doc: Document):Document = {
    doc.add(new Gene(0,0))
    doc
  }
}

class DummyDiseaseAnnotator extends Component {
  override def initialize() { println("Phenotype tagger initialized.") }
  override def preHook() { println("Start tagging disease...") }
  override def process(doc: Document):Document = {
    doc.add(new Phenotype(2,3))
    doc
  }
  override def postHook() { println("End tagging disease.") }
}

object Prototype extends App {
  implicit def componentToPipeline(component:Component):Pipeline = new Pipeline(component) //TODO: find a better place for this conversion
  val c1 = new DummyGeneAnnotator
  val c2 = new DummyDiseaseAnnotator
  val doc = new Document("This is a sample text.")
  val pipeline:Pipeline = c1 ++ c2 ++ c1
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
  result.annotations.foreach((a:Annotation) =>
    a match {
      //case s:Span => println(s.text)
      case _ => //
    }
  )
}