package simplexnlp.test

import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import simplexnlp.core.Document
import simplexnlp.core.Implicits._
import simplexnlp.example.Implicits._
import simplexnlp.example._

class Specs extends FunSpec with ShouldMatchers with GivenWhenThen {
  var doc:Document = _
  val ID = "doc423"
  //val text = "This disease is caused by the A54T substitution in gene XYZ. This is another sentence mentioning a disease and A54T."
  val text = "This is a-test s3ntence for (correct) tokenization."
  //initialize pipeline
  val s = new SentenceAnnotator
  s.parameters("path" -> "./ressources/OpenNLP/en-sent.bin")
  //val t = new FineTokenizer
  //val t = new WhiteSpaceTokenizer
  val t = new SimpleTokenizer
  val p = new POSTagger
  p.parameters("path" -> "./ressources/OpenNLP/en-pos-maxent.bin")
  val d = new DummyDiseaseAnnotator
  val c = new CoOccurrenceAnnotator
  doc = new Document(ID, text)
  val pipeline = s ++ t ++ p ++ d ++ c
  pipeline.initialize()
  pipeline.process(doc)

  describe("The pipeline") {
    it ("should have found two sentences") {
      pending
      doc.sentences.size should equal (2)
    }
    it ("each sentence should contain a mutation, a disease and a relation between both") {
      pending
      for (sentence <- doc.sentences) {
        sentence.diseases.size should equal (1)
        sentence.mutations.size should equal (1)
        sentence.relations.size should equal (1)
      }
    }
    it ("should tokenize correctly") {
      for (sentence <- doc.sentences)
        println(sentence.tokens)
    }
  }

  describe("An annotation") {
    it ("can have nested annotations") {
      pending
    }
  }
}