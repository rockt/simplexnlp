package simplexnlp.core

import simplexnlp.core.Util._

case class Result(TP: Int, FP: Int, FN: Int) {
  private def catchNaN(d: Double)(to: Double) = if (d.isNaN) to else d
  val P = round(catchNaN(TP.toDouble/(TP + FP))(1))(4)
  val R = round(catchNaN(TP.toDouble/(TP + FN))(0))(4)
  val F1 = round((2 * P * R)/(P + R))(4)
  def print() {
    println("TP\tFP\tFN\tP\tR\tF1\tSD")
    println(toString)
  }
  override def toString = "%d\t%d\t%d\t%.4f\t%.4f\t%.4f\t%.2f".format(TP, FP, FN, P, R, F1, sd)
  val sd = 0.0
}

abstract class AggregateResult(results:List[Result]) extends Result(
  results.map(_.TP).sum,
  results.map(_.FP).sum,
  results.map(_.FN).sum
)

//TODO: prevent doubled implementation of def +
case class MicroAvgResult(results:List[Result]) extends AggregateResult(results) {
  def +(that: MicroAvgResult) = MicroAvgResult(this.results ++ that.results)
}

case class MacroAvgResult(results:List[Result]) extends AggregateResult(results) {
  override val P = mean(_.P)
  override val R = mean(_.R)
  override val F1 = mean(_.F1)
  def mean(mapping: Result => Double) = results.map(mapping).sum/results.size
  def variance(mapping: Result => Double) = results.map(mapping).map(_ - mapping(this.asInstanceOf[Result])).map(math.pow(_, 2)).sum
  def sd(mapping: Result => Double):Double = math.sqrt(variance(mapping))
  def +(that: MacroAvgResult) = MacroAvgResult(this.results ++ that.results)
  override val sd:Double = round(sd(_.F1))(2)
}

//TODO: implement nested relations

//TODO: generalize this towards other metrics
abstract class Evaluator {
  var TP, FP, FN = 0
  var result:Result = _
  def evaluate(gold: Sentence, predicted: Sentence) //concrete class needs to provide an implementation
  def evaluate(goldCorpus:Corpus, predictedCorpus:Corpus):Result = {
    require(goldCorpus.size == predictedCorpus.size)
    val gold = goldCorpus.sortBy(_.id)
    val predicted = predictedCorpus.sortBy(_.id)
    for (i <- 0 until gold.size) {
      val goldDoc = gold(i)
      val predictedDoc = predicted.find(_.id == goldDoc.id).get
      require(goldDoc.sentences.size == predictedDoc.sentences.size)
      //TODO: what if we have to evaluate on the document-level?
      for (j <- 0 until goldDoc.sentences.size) {
        require(goldDoc.sentences(j).tokens.size == predictedDoc.sentences(j).tokens.size)
        evaluate(goldDoc.sentences(j), predictedDoc.sentences(j))
      }
    }
    Result(TP, FP, FN)
  }
}

class NEREvaluator[T <: Entity](implicit mf: Manifest[T]) extends Evaluator {
  //TODO: try soft bounds
  private def same(a:T, b:T):Boolean = a.start == b.start && a.end == b.end
  override def evaluate(gold: Sentence, predicted: Sentence) {
    val goldEntities = gold.children[T]
    val predictedEntities = predicted.children[T]
    val currentTP = goldEntities.filter((g:T) => predictedEntities.exists((p:T) => same(g, p))).size
    val currentFP = predictedEntities.filter((p:T) => !goldEntities.exists((g:T) => same(g, p))).size
    val currentFN = goldEntities.filter((g:T) => !predictedEntities.exists((p:T) => same(g, p))).size
    assert(currentTP + currentFP == predictedEntities.size)
    assert(currentTP + currentFN == goldEntities.size)
    TP += currentTP
    FP += currentFP
    FN += currentFN
  }
}