package simplexnlp.core

import simplexnlp.core.Util._
import simplexnlp.example._
import simplexnlp.example.Group
import simplexnlp.example.Drug
import simplexnlp.example.DrugN
import simplexnlp.example.Brand

case class Result(TP: Int, FP: Int, FN: Int) {
  private def catchNaN(d: Double)(to: Double) = if (d.isNaN) to else d
  val P = if (FP == 0) 1 else round(catchNaN(TP.toDouble/(TP + FP))(1))(4)
  val R = if (FN == 0) 1 else round(catchNaN(TP.toDouble/(TP + FN))(0))(4)
  val F1 = round((2 * P * R)/(P + R))(4)
  def print() {
    println("TP\tFP\tFN\tP\tR\tF1\tSD")
    println(toString)
  }
  override def toString = "%d\t%d\t%d\t%6.2f\t%6.2f\t%6.2f\t%.2f".format(TP, FP, FN, P*100, R*100, F1*100, sd)
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
  def evaluate(goldCorpus:Corpus, predictedCorpus:Corpus):Result =
    evaluate(goldCorpus.toArray, predictedCorpus.toArray)
  def evaluate(goldCorpus:Array[Document], predictedCorpus:Array[Document]):Result = {
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

class SpanEvaluator[T <: Span](implicit mf: Manifest[T]) extends Evaluator {
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

    //if (currentFP > 0)
      //println(predictedEntities.filter((p:T) => !goldEntities.exists((g:T) => same(g, p))).mkString("FP: ","\nFP: ",""))
    //if (currentFN > 0)
      //println(goldEntities.filter((g:T) => !predictedEntities.exists((p:T) => same(g, p))).mkString("FN: ","\nFN: ",""))

    TP += currentTP
    FP += currentFP
    FN += currentFN
  }
}

class MultiClassNEREvaluator {
  def evaluate(gold: Corpus, predicted: Corpus):List[(String, Result)] = {
    //FIXME: dirty, pass this as argument!
    val evaluator = new SpanEvaluator[Entity]
    val evaluatorDrug = new SpanEvaluator[Drug]
    val evaluatorDrugN = new SpanEvaluator[DrugN]
    val evaluatorGroup = new SpanEvaluator[Group]
    val evaluatorBrand = new SpanEvaluator[Brand]
    val evaluatorDisease = new SpanEvaluator[Disease]
    List(
      ("All",   evaluator.evaluate(gold, predicted)),
      ("Drug",  evaluatorDrug.evaluate(gold, predicted)),
      ("DrugN", evaluatorDrugN.evaluate(gold, predicted)),
      ("Group", evaluatorGroup.evaluate(gold, predicted)),
      ("Brand", evaluatorBrand.evaluate(gold, predicted)),
      ("Disease", evaluatorDisease.evaluate(gold, predicted))
    )
  }
}
class MultiClassDDIEvaluator {
  def evaluate(gold: Corpus, predicted: Corpus):List[(String, Result)] = evaluate(gold.toArray, predicted.toArray)
  def evaluate(gold: Array[Document], predicted: Array[Document]):List[(String, Result)] = {
    //FIXME: dirty, pass this as argument!
    val evaluator = new SpanEvaluator[DDI]
    val evaluatorEffect = new SpanEvaluator[DDIEffect]
    val evaluatorAdvise = new SpanEvaluator[DDIAdvise]
    val evaluatorMechanism = new SpanEvaluator[DDIMechanism]
    val evaluatorInt = new SpanEvaluator[DDIInt]
    List(
      ("All",   evaluator.evaluate(gold, predicted)),
      ("Effect",  evaluatorEffect.evaluate(gold, predicted)),
      ("Advise", evaluatorAdvise.evaluate(gold, predicted)),
      ("Mechanism", evaluatorMechanism.evaluate(gold, predicted)),
      ("Int", evaluatorInt.evaluate(gold, predicted))
    )
  }
}
