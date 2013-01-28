package simplexnlp.core

import simplexnlp.core.Util._
import collection.mutable.{ArrayBuffer, ListBuffer}

//TODO: class Workflow
//TODO: def |(that:Workflow)

//FIXME: figure out when to use Any, AnyRef or AnyVal!
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
  def overlapping[T <: Span](span: T)(implicit mf: Manifest[T]) =
    children[T].filter((t:T) => !(span.end < t.start || span.start > t.end))
  def preferLongerMatches(s1: Span, s2: Span) = s1.length > s2.length
  def preferSmallerMatches(s1: Span, s2: Span) = s1.length < s2.length
  //returns true iff span was added
  def addAndResolveOverlaps[T <: Span](span: T, resolver:(T,T) => Boolean)(implicit mf: Manifest[T]):Boolean = {
    val overlaps = overlapping[T](span)
    if (overlaps.isEmpty) { this + span.asInstanceOf[C]; true } //FIXME: this type-casting is ugly
    else if (overlaps.forall(resolver(span, _))) { overlaps.foreach(this - _.asInstanceOf[C]); this + span.asInstanceOf[C]; true }
    else false
  }
  def addAndResolveOverlaps[T <: Span](span: T)(implicit mf: Manifest[T]): Boolean =
    addAndResolveOverlaps[T](span, preferLongerMatches _)
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
      case doc: Document => doc
      case annot: Annotation => annot.doc
    }
  }
}

//a document with a text and annotations
class Document(val id: String, val text: String) extends Annotation with ParentOf[Annotation] {
  override def doc = this
  def sentences = children[Sentence]
  def coveredSpans[T <: Span](start:Int, end:Int)(implicit mf: Manifest[T]): List[T] =
    descendants[T].filter((s:T) => (s.startInDoc >= start && s.endInDoc <= end))
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
  def process():Corpus = {
    components.head match {
      case r: Reader => {
        val corpus = r.read
        corpus.foreach(process(_))
        corpus
      }
      case _ => throw new IllegalArgumentException("The first component has to be a Reader if you want to use this method!")
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

abstract class Reader extends Component with Parameters {
  def ++(that: Pipeline) = new Pipeline((List(this) ++ that.components): _*)
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
}

case class Token(start: Int, end: Int) extends NonOverlappingSpan {
  var pos = ""
  var index = 0
}

case class Entity extends Span {
  //TODO: not very nice, but works for filtering descendants by class
  override val start = 0
  override val end = 0
  var id = ""
  def className:String = this.getClass.getName.substring(this.getClass.getName.lastIndexOf('.') + 1)
}
trait NonOverlappingEntity extends Entity with NonOverlappingSpan

abstract class Relation(entities: Entity*) extends Span {
  //TODO: a relation might have a trigger word
  override val start = entities.sortBy(_.startInDoc).head.start
  override val end = entities.sortBy(_.endInDoc).last.end
}

abstract class BinaryRelation(var _1: Entity, var _2: Entity) extends Relation(_1, _2) {
  private val temp1 = _1
  private val temp2 = _2
  _1 = if (temp1.start <= temp2.start) temp1 else temp2
  _2 = if (temp1.start <= temp2.start) temp2 else temp1
}
