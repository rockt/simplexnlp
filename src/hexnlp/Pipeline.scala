package hexnlp

import collection.mutable.ListBuffer

//TODO: annotation should be connected to a document!
trait Annotation

class Document(val text:String) {
  var annotations = new ListBuffer[Annotation]
  def add(a:Annotation) = annotations.append(a)
  //TODO: def remove(a:Annotation)
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
}

//TODO: sentence should contain a list of tokens
case class Sentence(start:Int, end:Int) extends Span
case class Token(start:Int, end:Int) extends Span

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

class DummyAnnotator extends Component {
  override def process(text: Document):Document = {
    text.add(new Gene(0,0))
    text
  }
}

class AnotherDummyAnnotator extends Component {
  override def initialize() {println("Disease tagger initialized!")}
  override def process(text: Document):Document = {
    text.add(new Disease(2,3))
    text
  }
  override def postHook() {println("Disease tagger finished!")}
}

object Prototype extends App {
  implicit def componentToPipeline(component:Component):Pipeline = new Pipeline(component)
  val c1 = new DummyAnnotator
  val c2 = new AnotherDummyAnnotator
  val doc = new Document("This is a sample text.")
  val pipeline:Pipeline = c1 ++ c2 ++ c1
  val result = pipeline.process(doc)
  val relation = PPI(Gene(1,2), Gene(2,3))
  println(pipeline)
  println(doc.text)
  println(result)
  println(relation)
}