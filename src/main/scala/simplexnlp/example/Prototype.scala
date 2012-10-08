package simplexnlp.example

import simplexnlp.core._
import simplexnlp.core.{Sentence => GenericSentence}
import opennlp.tools.sentdetect.{SentenceModel, SentenceDetectorME}
import java.io.FileInputStream
import simplexnlp.example.Implicits._
import opennlp.tools.postag.{POSModel, POSTaggerME}

//example NLP pipeline
case class Mutation(start: Int, end: Int) extends Entity
case class Gene(start: Int, end: Int) extends Entity
case class Disease(start: Int, end: Int) extends Entity {
  var id:String = ""
}

case class Drug(start: Int, end: Int) extends Entity

case class PPI(a: Gene, b: Gene) extends Relation(a, b) {
  override def toString: String = "PPI: " + a + " - " + b
}
case class DDI(a: Drug, b: Drug) extends Relation(a, b) {
  override def toString: String = "DDI: " + a + " - " + b
}
case class MutationDiseaseRelation(m: Mutation, d: Disease) extends Relation(m, d) {
  //override def toString: String = "MutationDiseaseRelation: " + m + " - " + d
}

case class GeneDiseaseRelation(g: Gene, d: Disease) extends Relation(g, d) {
  override def toString: String = "GDI: " + g + " - " + d + " => " + text
}

case class Sentence(override val start: Int, override val end: Int) extends GenericSentence(start, end) {
  def genes = children[Gene]
  def mutations = children[Mutation]
  def diseases = children[Disease]
}

class SentenceAnnotator extends Component with Parameters {
  var tagger:SentenceDetectorME = _
  override def initialize {
    tagger = new SentenceDetectorME(new SentenceModel(new FileInputStream(parameters[String]("path"))))
  }
  override def process(doc: Document) {
    val spans = tagger.sentPosDetect(doc.text)
    for (span: opennlp.tools.util.Span <- spans) {
      doc + Sentence(span.getStart, span.getEnd-1)
    }
  }
}

class POSTagger extends Component with Parameters {
  var tagger:POSTaggerME = _
  override def initialize {
    tagger = new POSTaggerME(new POSModel(new FileInputStream(parameters[String]("path"))))
  }
  override def process(doc: Document) {
    for (sentence <- doc.sentences) {
      val tokens = sentence.tokens
      val tags = tagger.tag(tokens.map(_.text).toArray)
      for (i <- 0 until tokens.size) tokens(i).pos = tags(i)
    }
  }
}

class FineTokenizer extends Component {
  override def process(doc: Document) = {
    //TODO: think of a more functional implementation
    for (sentence <- doc.sentences) {
      val chars = sentence.text.toCharArray
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

class SimpleTokenizer extends Component {
  def process(doc: Document) = {
    //TODO: think of a more functional implementation
    for (sentence <- doc.sentences) {
      val chars = sentence.text.toCharArray
      var start = 0
      for (i <- 0 until chars.length) {
        val ch = chars(i)
        var nch:Char = ' '
        if (i < chars.length - 1) nch = chars(i + 1)
        if (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r') {
          start = start + 1
        } else if ((Character.isLetterOrDigit(ch) && !Character.isLetterOrDigit(nch)) ||
        (!Character.isLetterOrDigit(ch) && Character.isLetterOrDigit(nch))) {
          sentence + Token(start, i)
          start = i + 1
        } else if (nch == ' ' || nch == '\t' || nch == '\n' || nch == '\r') {
          sentence + Token(start, i)
          start = i + 1
        } else {
          //wait for next whitespace
        }
      }
    }
  }
}

class WhiteSpaceTokenizer extends Component {
  def process(doc: Document) {
    //TODO: think of a more functional implementation
    for (sentence <- doc.sentences) {
      val chars = sentence.text.toCharArray
      var start = 0
      for (i <- 0 until chars.length) {
        val ch = chars(i)
        var nch:Char = ' '
        if (i < chars.length - 1) nch = chars(i + 1)
        if (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r') {
          start = start + 1
        } else if (nch == ' ' || nch == '\t' || nch == '\n' || nch == '\r') {
          sentence + Token(start, i)
          start = i + 1
        } else {
          //wait for next whitespace
        }
      }
    }
  }
}

class DummyDiseaseAnnotator extends Component {
  val DISEASE = "disease"
  override def process(doc: Document) = {
    for (sentence <- doc.sentences) {
      val position = sentence.text.indexOf(DISEASE)
      if (position >= 0) sentence + Disease(position, position + DISEASE.length - 1)
    }
  }
}

class GeneAnnotator extends Component {
  override def process(doc: Document) = {
    //TODO: use GeneTUKit here
  }
}

class CoOccurrenceAnnotator extends Component {
  override def process(doc: Document) = {
    for (sentence <- doc.sentences; mutation <- sentence.mutations; disease <- sentence.diseases) {
      sentence + MutationDiseaseRelation(mutation, disease)
    }
  }
}