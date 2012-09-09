package simplexnlp

import edu.uchsc.ccp.nlp.ei.mutation.MutationFinder
import simplexnlp.Util._
import simplexnlp.Implicits._

/**
 * User: Tim Rocktaeschel
 * Date: 9/9/12
 * Time: 3:24 PM
 */

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