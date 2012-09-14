package simplexnlp.core

import simplexnlp.core.Util._
import collection.mutable.{ArrayBuffer, ListBuffer}

//TODO: class Workflow
//TODO: def |(that:Workflow)

//something that has children
trait ParentOf[C <: Child] {
  private val childrenBuffer = new ListBuffer[C]
  def add(child: C) = {
    childrenBuffer += child
    child.parent = this
  }
  def remove(child: C) = {
    childrenBuffer - child
    child.parent = null
  }
  def +(child: C) = add(child)
  def -(child: C) = remove(child)
  private def gatherDescendants: List[C] = {
    val buffer = new ListBuffer[C]
    for (child <- this.asInstanceOf[ParentOf[C]].childrenBuffer) {
      buffer.append(child)
      //recursion
      if (child.isInstanceOf[ParentOf[C]]) buffer.appendAll(child.asInstanceOf[ParentOf[C]].gatherDescendants)
    }
    buffer.toList
  }
  def children[T](implicit mf: Manifest[T]) = filterByType[T](childrenBuffer.toList)
  def descendants[T](implicit mf: Manifest[T]) = filterByType[T](gatherDescendants)
}

//something that has a parent
trait Child {
  var parent: Any = _
}

//stores predictions by NLP components
trait Prediction {
  private var pred:Any = _
  def prediction_=(p:Any) = { pred = p }
  def prediction[T]():T = pred.asInstanceOf[T]
}

//an annotation refers to a document and might be nested in another annotation
trait Annotation extends Child with Prediction {
  //TODO: we need a flag that distinguishes between predictions and goldstandard
  //get the document (root ancestor)
  def doc: Document = {
    if (parent.isInstanceOf[Document]) parent.asInstanceOf[Document]
    //recursion
    else parent.asInstanceOf[Annotation].doc
  }
}



//a document with a text and annotations
class Document(val id: String, val text: String) extends Annotation with ParentOf[Annotation] {
  override def doc = this
  def sentences = children[Sentence]
  //FIXME: should be generic
  //FIXME: infinity loop???
  def coveredSpans(start:Int, end:Int):List[Span] = descendants[Span].filter((s:Span) => (s.startInDoc >= start && s.endInDoc <= end))
}

class Corpus extends ArrayBuffer[Document] {
  def shuffled(seed: Int):Array[Document] = {
    val random = new scala.util.Random(seed)

    //Fisher Yates Shuffle from: http://jdleesmiller.blogspot.de/2008/12/shuffles-surprises-and-scala.html
    def fisherYatesShuffle[T](xs: Array[T]) = {
      for (i <- xs.indices.reverse)
        swap(xs, i, random.nextInt(i + 1))
    }

    def swap[T](xs: Array[T], i: Int, j: Int) = {
      val t = xs(i)
      xs(i) = xs(j)
      xs(j) = t
    }

    val temp = this.toArray
    fisherYatesShuffle(temp)
    temp
  }

  //TODO: def splits(number: Int, seed:Int)
}

//TODO: implement Input und Output type specification
//a NLP component
abstract class Component {
  //a concrete component needs to override this method
  def process(doc: Document)
  def initialize() {
    //something to do before start processing (e.g. loading parameters)
  }
  def preHook() {
    //something to do before each call of process
  }
  def postHook() {
    //something to do after each call of process
  }
}

//FIXME: uptil now we no real pipelining!
//a chain of NLP components
class Pipeline(comps: Component*) {
  var components = comps.toList
  def ++(that: Pipeline) = new Pipeline((this.components ++ that.components): _*)
  def process(doc: Document):Unit = {
    components.foreach((c: Component) => {
      c.preHook()
      c.process(doc)
      c.postHook()
    })
  }
  def process():Unit = {
    components.head match {
      case r:Reader => r.process()
      case _ => throw new IllegalStateException("The first component has to be a Reader if you want to use this method!")
    }
  }
  override def toString = components.map(getClassName(_)).mkString("*Input* => " +
    (if (this.isInstanceOf[Reader]) getClassName(this) + " -> " else ""), " -> ", " => *Output*")
  def initialize() = { components.foreach(_.initialize()) }
}

//TODO: there has to be a more elegant way
trait Parameters {
  import scala.collection.mutable.HashMap
  private val params = new HashMap[String, AnyVal]
  def parameters(tuple: Tuple2[String, _]*): Unit = {
    tuple.foreach((t: Tuple2[String, _]) => params.put(t._1, t._2.asInstanceOf[AnyVal]))
  }
  def parameters[T](key:String):T = params(key).asInstanceOf[T]
}


//TODO: think of a more elegant implementation!
abstract class Reader extends Pipeline with Parameters {
  def read:Corpus = read(parameters[String]("path"))
  //TODO: would be nicer to have an Iterator here!
  def read(path:String):Corpus
  def documents:Corpus = {
    val docs = read
    for (doc <- docs) super.process(doc)
    docs
  }
  override def process():Unit = for (doc <- read) super.process(doc)
  override def ++(that: Pipeline):Pipeline = {
    components = this.components ++ that.components
    this
  }
  override def process(doc:Document) = {
    //do nothing
  }
}

trait Span extends Annotation {
  val start: Int
  val end: Int
  assert(end - start >= 0, "A span must start before it ends!")
  //TODO: def append
  //TODO: def prepend
  //TODO: def trimStart
  //TODO: def trimEnd
  def enclosingText:String = {
    parent match {
      case span:Span => span.text
      case doc:Document => doc.text
    }
  }
  def startInDoc:Int = {
    parent match {
      case span:Span => span.startInDoc + start
      case doc:Document => start
    }
  }
  def endInDoc:Int = {
    parent match {
      case span:Span => span.startInDoc + end
      case doc:Document => end
    }
  }
  //FIXME: should be generic
  def covered = doc.coveredSpans(startInDoc, endInDoc)
  def text = doc.text.substring(startInDoc, endInDoc+1)
  def length = text.length
  override def toString = getClassName(this) + "[" + start + "-" + end + "]: " + text
}

trait NonOverlappingSpan extends Span with Ordered[NonOverlappingSpan] {
  override def compare(that: NonOverlappingSpan): Int = {
    assert((this.start < that.start || this.start > that.start) && (this.end <= that.start || this.start >= that.end), this + " overlaps " + that)
    this.start - that.start
  }
}

case class Sentence(start: Int, end: Int) extends NonOverlappingSpan with ParentOf[Annotation] {
  def tokens = children[Token]
  def entities = children[Entity]
  def relations = children[Relation]
  private var numberOfTokens = 0
  override def add(child: Annotation) = {
    if (child.isInstanceOf[Token]) {
      child.asInstanceOf[Token].index = numberOfTokens
      numberOfTokens += 1
    }
    super.add(child)
  }
}

case class Token(start: Int, end: Int) extends NonOverlappingSpan {
  var pos: String = _
  var index: Int = _
}

trait Entity extends Span

abstract class Relation(entities: Entity*) extends Span {
  //TODO: a relation might have a trigger word
  val start = entities.sortBy(_.startInDoc).head.start
  val end = entities.sortBy(_.endInDoc).last.end
}

//TODO: implement nested relations


abstract class Evaluator {
  var TP:Double = 0
  var FP:Double = 0
  var FN:Double = 0
  def P = TP/(TP + FP)
  def R = FN/(TP + FN)
  def F1 = (2 * P * R)/(P + R)

  def evaluate(gold: Sentence, predict: Sentence)

  def evaluate(gold:Corpus, predict:Corpus) {
    assert(gold.size == predict.size)
    for (i <- 0 until gold.size) {
      assert(gold(i).sentences.size == predict(i).sentences.size)
      for (j <- 0 until gold(i).sentences.size) {
        assert(gold(i).sentences(j).tokens.size == predict(i).sentences(j).tokens.size)
        evaluate(gold(i).sentences(j), predict(i).sentences(j))
      }
    }
  }

  def performKFoldCV(folds: Int, corpus:Corpus, pipeline:Pipeline) {
    //TODO
  }

  def printResults = {
    println("%f\t%f\t%f\t%.2f\t%.2f\t%.2f".format(TP, FP, FN, P, R, F1))
  }
}