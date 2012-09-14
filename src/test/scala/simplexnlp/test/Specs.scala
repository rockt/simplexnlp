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
  val text = "This disease is caused by the A54T substitution in gene XYZ. This is another sentence mentioning a disease and A54T."
  //initialize pipeline
  val s = new SentenceAnnotator
  s.parameters("pathToModelFile" -> "./ressources/OpenNLP/en-sent.bin")
  val t = new FineTokenizer
  val m = new MutationAnnotator
  val d = new DummyDiseaseAnnotator
  val c = new CoOccurrenceAnnotator
  m.parameters("pathToRegEx" -> "./ressources/mutationFinder/regex.txt")
  doc = new Document(ID, text)
  val pipeline = s ++ t ++ m ++ d ++ c
  pipeline.initialize()
  pipeline.process(doc)

  describe("The pipeline") {
    it ("should have found two sentences") {
      doc.sentences.size should equal (2)
    }
    it ("each sentence should contain a mutation, a disease and a relation between both") {
      for (sentence <- doc.sentences) {
        sentence.diseases.size should equal (1)
        sentence.mutations.size should equal (1)
        sentence.relations.size should equal (1)
      }
    }
  }

  describe("An annotation") {
    it ("can have nested annotations") {
      pending
    }
  }
}