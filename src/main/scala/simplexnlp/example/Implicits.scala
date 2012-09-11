package simplexnlp.example

object Implicits {
  implicit def senenceToSentence(genSen: simplexnlp.core.Sentence):Sentence = genSen.asInstanceOf[Sentence]
}