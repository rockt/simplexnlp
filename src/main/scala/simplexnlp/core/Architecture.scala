package simplexnlp.core

import simplexnlp.core.Util._
import collection.mutable.{ArrayBuffer, ListBuffer}

//TODO: class Workflow
//TODO: def |(that:Workflow)

//FIXME: determine when to use Any, AnyRef or AnyVal!

//something that has children
trait ParentOf[C <: Child] {
  private val childrenBuffer = new ListBuffer[C]
  def add(child: C) = {
    //TODO: not very elegant to do this here
    child match {
      case thiz:NonOverlappingSpan => {
        type T = thiz.type
        require(descendants[T].forall((that:T) => thiz.end < that.start || thiz.start > that.end), {
          val overlap = descendants[T].find((that:T) => !(thiz.end < that.start || thiz.start > that.end)).get
          "New span annotation [" + thiz.start + "-" + thiz.end + "]" + " overlaps with " + overlap + " in\n" + this
        })
      }
      case _ => //proceed
    }
    childrenBuffer += child
    child.parent = this
  }
  def remove(child: C) = {
    if (childrenBuffer.contains(child)) {
      childrenBuffer -= child
      child.parent = null
    }
  }
  def +(child: C) = add(child)
  def -(child: C) = remove(child)
  private def gatherDescendants: List[C] = {
    val buffer = new ListBuffer[C]
    for (child:C <- childrenBuffer) {
      buffer.append(child)
      child match {
        case parent:ParentOf[C] => buffer.appendAll(parent.gatherDescendants)
        case _ => //proceed
      }
    }
    buffer.toList
  }
  def children[T](implicit mf: Manifest[T]):List[T] = filterByType[T](childrenBuffer.toList)
  def descendants[T](implicit mf: Manifest[T]):List[T] = filterByType[T](gatherDescendants)
  private def deepCopy[A](a: A)(implicit m: reflect.Manifest[A]): A = util.Marshal.load[A](util.Marshal.dump(a))
  //slow but beautiful
  def copy(implicit m: reflect.Manifest[this.type]):this.type = deepCopy[this.type](this)
  def copyAndFilter[T <: ScalaObject](types: T*)(implicit m: scala.reflect.Manifest[T]):this.type = {
    val temp = this.copy
    temp.removeChildrenByTypes(types: _*)
    temp
  }
  def removeChildrenByTypes[T <: ScalaObject](types: T*)(implicit m: scala.reflect.Manifest[T]): Unit =
    for (child <- childrenBuffer)
      if (types.exists(_.getClass.getName.dropRight(1) == child.getClass.getName)) this - child
      else child match {
        case parent:ParentOf[_] => parent.removeChildrenByTypes(types: _*)
        case _ => //keep annotation
      }
}

//something that has a parent
trait Child extends Serializable {
  var parent: AnyRef = _ //TODO: better Any?
}

//an annotation refers to a document and might be nested in another annotation
trait Annotation extends Child {
  //get the document (root ancestor)
  def doc: Document = {
    parent match {
      case doc:Document => doc
      case annot:Annotation => annot.doc
    }
  }
}

//a document with a text and annotations
class Document(val id: String, val text: String) extends Annotation with ParentOf[Annotation] {
  override def doc = this
  def sentences = children[Sentence]
  def coveredSpans[T <: Span](start:Int, end:Int)(implicit mf: Manifest[T]):List[T] = descendants[T].filter((s:T) => (s.startInDoc >= start && s.endInDoc <= end))
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
  def round[T](l: List[T], n: Int) = (0 until n).map{ i => l.drop(i).sliding(1, n).flatten.toList }.toList
  //TODO: this should return an Array[Corpus]
  def split(parts: Int, seed:Int):Array[Array[Document]] = {
    val temp = shuffled(seed)
    val splits = round(temp.toList, parts)
    splits.map(_.toArray).toArray
  }
}

//TODO: implement Input und Output type specification
//a NLP component
abstract class Component {
  def process(doc: Document) //a concrete component needs to override this method
  def initialize() { } //something to do before start of processing (e.g. loading parameters)
  def preHook() { } //something to do before each call of process
  def postHook() { } //something to do after each call of process
}

//FIXME: until now there is no real pipelining! use actors!
//a chain of NLP components
class Pipeline(val components: Component*) {
  def ++(that: Pipeline) = new Pipeline((this.components ++ that.components): _*)
  def process(doc: Document):Unit = {
    components.foreach((c: Component) => {
      if (!c.isInstanceOf[Reader]) {
        c.preHook()
        c.process(doc)
        c.postHook()
      }
    })
  }
  def process():Unit = {
    components.head match {
      case r: Reader => r.read.foreach(process(_))
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
  private val params = new HashMap[String, Any]
  def parameters(tuple: (String, _)*) =
    tuple.foreach((t: (String, _)) => params.put(t._1, t._2.asInstanceOf[Any]))
  //this assumes that you know what you are doing when asking for a parameter of a specific type
  def parameters[T](key:String):T = params(key).asInstanceOf[T]
  def add(t: (String, _)) = params.put(t._1, t._2.asInstanceOf[Any])
  def print() = for (key <- params.keySet.toList.sorted) println(key + " " + params(key))
  def contains(key: String) = params.contains(key)
  def hashId = params.values.mkString(".").hashCode
}

abstract class Reader extends Pipeline with Parameters {
  def read:Corpus = read(parameters[String]("path"))
  def read(path:String):Corpus
  //maybe just do nothing?
  override def process(doc:Document) = throw new IllegalArgumentException("A reader does not process documents!")
}

trait Span extends Annotation {
  val start: Int
  val end: Int
  require(start <= end, "A span must start before it ends!")
  //TODO: def append
  //TODO: def prepend
  //TODO: def trimStart
  //TODO: def trimEnd
  def enclosingText:String = parent match {
    case span:Span => span.text
    case doc:Document => doc.text
  }
  def startInDoc:Int = parent match {
    case span:Span => span.startInDoc + start
    case doc:Document => start
  }
  def endInDoc:Int = parent match {
    case span:Span => span.startInDoc + end
    case doc:Document => end
  }
  def covered[T <: Span](implicit mf: Manifest[T]) = doc.coveredSpans[T](startInDoc, endInDoc)
  def text = doc.text.substring(startInDoc, endInDoc+1)
  def length = end - start
  override def toString = getClassName(this) + "[" + start + "-" + end + "]: " + text
}

trait NonOverlappingSpan extends Span with Ordered[NonOverlappingSpan] {
  override def compare(that: NonOverlappingSpan): Int = this.start - that.start
}

case class Sentence(start: Int, end: Int) extends Span with ParentOf[Annotation] {
  def tokens = children[Token]
  def entities = children[Entity]
  def relations = children[Relation]
  private var numTokens = 0
  override def add(child: Annotation) = {
    child match {
      case token:Token => { token.index = numTokens; numTokens += 1 }
      case _ => //proceed
    }
    super.add(child)
  }
  def overlapping[T <: Span](span: T)(implicit mf: Manifest[T]) =
    children[T].filter((t:T) => !(span.end < t.start || span.start > t.end))
  def preferLongerMatches(s1: Span, s2: Span) = s1.length > s2.length
  //returns true iff span was added
  def addAndResolveOverlaps[T <: Span](span: T, resolver:(T,T) => Boolean)(implicit mf: Manifest[T]):Boolean = {
    val overlaps = overlapping[T](span)
    if (overlaps.isEmpty) { this + span; true }
    else if (overlaps.forall(resolver(span, _))) { overlaps.foreach(this - _); this + span; true }
    else false
  }
  def addAndResolveOverlaps[T <: Span](span: T)(implicit mf: Manifest[T]):Boolean = addAndResolveOverlaps[T](span, preferLongerMatches _)
}

case class Token(start: Int, end: Int) extends NonOverlappingSpan {
  var pos = ""
  var index = 0
}

trait Entity extends Span { var id = "" }
trait NonOverlappingEntity extends Entity with NonOverlappingSpan

abstract class Relation(entities: Entity*) extends Span {
  //TODO: a relation might have a trigger word
  override val start = entities.sortBy(_.startInDoc).head.start
  override val end = entities.sortBy(_.endInDoc).last.end
}


case class Result(TP: Int, FP: Int, FN: Int) {
  lazy val P = TP.toDouble/(TP + FP)
  lazy val R = TP.toDouble/(TP + FN)
  lazy val F1 = (2 * P * R)/(P + R)
  def print() {
    println("TP\tFP\tFN\tP\tR\tF1")
    println(toString)
  }
  override def toString = "%d\t%d\t%d\t%.4f\t%.4f\t%.4f".format(TP, FP, FN, P, R, F1)
}

case class MicroAvgResult(results:List[Result]) extends Result(
  results.map(_.TP).sum,
  results.map(_.FP).sum,
  results.map(_.FN).sum
) {
  def +(that: MicroAvgResult) = MicroAvgResult(this.results ++ that.results)
}

//TODO: find a better way than calling constructor with dummy objects
case class MacroAvgResult(results:List[Result]) extends Result(1, 0, 0) {
  override lazy val P = mean(_.P)
  override lazy val R = mean(_.R)
  override lazy val F1 = mean(_.F1)
  def mean(mapping: Result => Double) = results.map(mapping).sum/results.size
  def variance(mapping: Result => Double) = results.map(mapping).map(_ - mapping(this.asInstanceOf[Result])).map(math.pow(_, 2)).sum
  def sd(mapping: Result => Double) = math.sqrt(variance(mapping))
  def +(that: MacroAvgResult) = MacroAvgResult(this.results ++ that.results)
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
    FP += currentFP
    FN += currentFN
  }
}