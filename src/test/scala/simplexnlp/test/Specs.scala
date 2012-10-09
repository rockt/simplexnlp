package simplexnlp.test

import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import simplexnlp.core.{Token, Document}
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
      pending
      for (sentence <- doc.sentences)
        println(sentence.tokens)
    }
  }

  describe("An annotation") {
    it ("can have nested annotations") {
      pending
    }
  }

  describe("Span") {
    it ("has correct length") {
      pending
      val text = "This is a test."
      val doc = new Document("0", text)
      val sentence = new Sentence(0, text.length)
      doc + sentence
      val s1 = new Disease(0,4)
      val s2 = new Disease(8,9)
      sentence + s1
      sentence + s2
      assert(s1.length === 4)
      assert(s2.length === 1)
      val overlap = new Disease(8,14)
      sentence.addAndResolveOverlaps(overlap)
      assert(sentence.diseases(1) === overlap)
    }
  }

  describe("Parent") {
    it("copies correctly") {
      val text = "This is a test."
      val doc = new Document("0", text)
      val sentence = new Sentence(0, text.length-1)
      doc + sentence
      sentence + Gene(5,6)
      sentence + Token(0,3)
      sentence + Token(5,6)
      sentence + Token(8,8)
      sentence + Token(10,13)
      sentence + Token(14,14) //FIXME: here is a bug!
      sentence + Disease(0,3)
      sentence + Disease(8,8)
      val docCopy:Document = doc.copyAndFilter(Gene)
      val d1 = doc.descendants.last.asInstanceOf[Disease]
      d1.id = "foo"
      val d2 = docCopy.descendants.last.asInstanceOf[Disease]
      d2.id = "bar"
      println(d1 + " " + doc.descendants.last.asInstanceOf[Disease].id)
      println(d2 + " " + docCopy.descendants.last.asInstanceOf[Disease].id)
      doc.removeChildrenByTypes(Token)
      println(doc.descendants)
      println(docCopy.descendants)
    }
  }
}