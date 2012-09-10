package simplexnlp.example

import simplexnlp.core._
import edu.uchsc.ccp.nlp.ei.mutation.MutationFinder
import simplexnlp.core.Util._
import simplexnlp.core.Implicits._
import simplexnlp.core.{Sentence => GenericSentence}
import opennlp.tools.sentdetect.{SentenceModel, SentenceDetectorME}
import java.io.FileInputStream

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

case class Sentence(override val start: Int, override val end: Int) extends GenericSentence(start, end) {
  def genes = childrenFilteredBy[Gene]
  def mutations = childrenFilteredBy[Mutation]
  def diseases = childrenFilteredBy[Disease]
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
  var tagger: MutationFinder = _
  override def initialize {
    suppressConsoleOutput {
      tagger = new MutationFinder("./resources/mutationFinder/regex.txt")
    }
  }
  override def process(doc: Document) = {
    import scala.collection.JavaConversions._
    for (sentence <- doc.sentences) {
      val mutations = tagger.extractMutations(sentence.text)
      for (mutation <- mutations.keySet(); tuple <- mutations.get(mutation)) {
        val span = tuple.asInstanceOf[Array[Int]]
        sentence + Mutation(span(0), span(1))
      }
    }
  }
}

class SentenceDetector extends Component {
  var tagger: SentenceDetectorME = _
  override def initialize {
    tagger = new SentenceDetectorME(new SentenceModel(new FileInputStream("./resources/OpenNLP/SentDetectGenia.bin.gz")))
  }
  override def process(doc: Document) {
    val spans = tagger.sentPosDetect(doc.text)
    for (span:opennlp.tools.util.Span <- spans) {
      doc + Sentence(span.getStart, span.getEnd)
    }
  }
}

class BANNERAnnotator extends Component {
  override def process(doc: Document) = {
    //TODO
  }
}

class CoOccurrenceAnnotator extends Component {
  override def process(doc: Document) = {
    //TODO
  }
}

object Prototype extends App {
  val s = new SentenceDetector
  val d = new DummyDiseaseAnnotator
  val m = new MutationAnnotator
  val doc = new Document("This disease is caused by the A54T substitution in gene XYZ. This is another sentence.")
  val pipeline = s -> d -> m -> d
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