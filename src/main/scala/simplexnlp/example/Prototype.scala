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

class FineTokenizer extends Component {
  override def process(doc: Document) = {
    //TODO: think of a more functional implementation
    for (sentence <- doc.sentences) {
      val chars = doc.text.toCharArray
      var start = 0
      for (i <- 0 until chars.length) {
        val ch = chars(i)
        var nch:Char = ' '
        if (i < chars.length - 1) nch = chars(i + 1)
        if (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r') {
          start = start + 1
        } else {
          if (Character.isDigit(ch) && !Character.isDigit(nch)) {
            sentence + Token(start, i)
            start = i + 1
          } else if (Character.isLetter(ch) && !Character.isLetter(nch)) {
            sentence + Token(start, i)
            start = i + 1
          } else if (!(Character.isDigit(ch) || Character.isLetter(ch))) {
            sentence + Token(start, i)
            start = i + 1
          }
        }
      }
    }
  }
}

class MutationAnnotator extends Component with Parameters {
  var tagger: MutationFinder = _
  override def initialize {
    suppressConsoleOutput {
      tagger = new MutationFinder(parameters("pathToRegEx").asInstanceOf[String])
    }
  }
  override def process(doc: Document) = {
    import scala.collection.JavaConversions._
    for (sentence <- doc.sentences; mutations = tagger.extractMutations(sentence.text); mutation <- mutations.keySet(); tuple <- mutations.get(mutation)) {
      val span = tuple.asInstanceOf[Array[Int]]
      sentence + Mutation(span(0), span(1)-1)
    }
  }
}

class SentenceAnnotator extends Component with Parameters {
  var tagger:SentenceDetectorME = _
  override def initialize {
    tagger = new SentenceDetectorME(new SentenceModel(new FileInputStream(parameters("pathToModelFile").asInstanceOf[String])))
  }
  override def process(doc: Document) {
    val spans = tagger.sentPosDetect(doc.text)
    for (span: opennlp.tools.util.Span <- spans) {
      doc + Sentence(span.getStart, span.getEnd-1)
    }
  }
}

class GeneAnnotator extends Component {
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
  val s = new SentenceAnnotator
  s.parameters("pathToModelFile" -> "./resources/OpenNLP/en-sent.bin")
  val t = new FineTokenizer
  val m = new MutationAnnotator
  m.parameters("pathToRegEx" -> "./resources/mutationFinder/regex.txt")
  val doc = new Document(0, "This disease is caused by the A54T substitution in gene XYZ. This is another sentence.")
  val pipeline = s -> t -> m
  pipeline.initialize()
  pipeline.process(doc)
  println("Pipeline:\t" + pipeline)
  println("Text:\t\t" + doc.text)
  println("Sentences:")
  println(doc.sentences)
  println("Sentence descendants")
  implicit def genSenToSen(genSen: GenericSentence):Sentence = genSen.asInstanceOf[Sentence] //TODO: find a better place for implicits (preferrably within the class)
  for (sentence <- doc.sentences) {
    println(sentence.mutations)
  }
  println("All Annotations:")
  println(doc.descendants)
}