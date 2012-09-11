package simplexnlp.example

import simplexnlp.core.{Sentence => GenericSentence}

object Implicits {
  implicit def genSenToSen(genSen: GenericSentence):Sentence = genSen.asInstanceOf[Sentence]
}