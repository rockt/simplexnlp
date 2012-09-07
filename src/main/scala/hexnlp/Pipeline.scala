package hexnlp

import collection.mutable.ListBuffer
import edu.uchsc.ccp.nlp.ei.mutation.MutationFinder
import hexnlp.Util._
import hexnlp.Implicits._

trait ParentOf[C <: Child] {
  val children = new ListBuffer[C]
  def +(child:C) = {
    children += child
    child.parent = this
  }
  def add(child:C) = this + child
  def -(child:C) = {
    children - child
    child.parent = null
  }
  def remove(child:C) = this - child
  def filteredChildren[T](implicit mf:Manifest[T]) : List[T] = {
     children.collect({
       case t if mf.erasure.isAssignableFrom(t.getClass) => t
     }).asInstanceOf[ListBuffer[T]].toList
   }
}

trait Child {
  var parent:Any = _
}

trait Annotation extends Child {
  //for now... but it is a hack!
  def doc = parent.asInstanceOf[Document] //just an alias
  //gathers all annotations recursively
  def annotations:List[Annotation] = {
    if (this.isInstanceOf[ParentOf[Annotation]]) {
      val buffer = new ListBuffer[Annotation]
      buffer.append(this)
      for (child <- this.asInstanceOf[ParentOf[Annotation]].children) {
        buffer.appendAll(child.annotations)
      }
      buffer.toList
    } else List(this)

  }
}

//a mutable document with annotations
//TODO: every document needs an ID
class Document(val text:String) extends Annotation with ParentOf[Annotation] {
  override def doc = this
  def sentences = filteredChildren[Sentence]
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
  components.appendAll(cs) //FIXME: that is silly
  def ++(that:Pipeline) = new Pipeline((this.components ++ that.components): _*)
  //TODO: def |(that:Pipeline)
  def process(doc:Document) = {
    components.foreach((c:Component) => {
      c.preHook()
      c.process(doc)
      c.postHook()
      })
  }
  override def toString = components.toString()
}

trait Span extends Annotation {
  val start:Int
  val end:Int
  assert(end - start >= 0, "A span must start before it ends!")
  //TODO: def append
  //TODO: def prepend
  //TODO: def trimStart
  //TODO: def trimEnd
  def text = doc.text.substring(start, end)
  def length = text.length
  override def toString = this.getClass.toString.substring(this.getClass.toString.lastIndexOf('.')+1) + "[" + start + "-" + end + "]: " + text
}

trait NonOverlappingSpan extends Span with Ordered[NonOverlappingSpan] {
  override def compare(that:NonOverlappingSpan):Int = {
    assert((this.start < that.start || this.start > that.start) && (this.end <= that.start || this.start >= that.end), this + " overlaps " + that)
    this.start - that.start
  }
}

case class Sentence(start:Int, end:Int) extends NonOverlappingSpan with ParentOf[Span] {
  def tokens = filteredChildren[Token]
  def entities = filteredChildren[Entity]
  def genes = filteredChildren[Gene]
  def mutations = filteredChildren[Mutation]
  def diseases = filteredChildren[Disease]
}

case class Token(start:Int, end:Int) extends NonOverlappingSpan with Child { var pos:String = _ }

trait Entity extends Span with Child

abstract class Relation(entities:Entity*) extends Span {
  //TODO: find start of first and end of last entity instead
  val start = entities.head.start
  val end = entities.head.end
  //TODO: a relation might have a trigger word
}

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
    if (result >= 0) doc + Disease(result, result + DISEASE.length)
  }
}

class DummySentenceAnnotator extends Component {
  override def process(doc: Document) = {
    doc + Sentence(0, doc.text.length)
  }
}

class MutationAnnotator extends Component {
  var extractor:MutationFinder  = _
  override def initialize() {
    suppressConsoleOutput {
      extractor = new MutationFinder("./resources/mutationFinder/regex.txt")
    }
  }

  override def process(doc:Document) = {
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
  override def process(doc:Document) = {
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
  println("All Annotations:")
  println(doc.annotations) //FIXME: what goes wrong here???
}