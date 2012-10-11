package simplexnlp.core

import java.io.{ByteArrayOutputStream, PrintStream}
import java.util.Random

object Util {
  def random = new Random(1337)

  def suppressConsoleOutput(body: => Any) = {
    val temp = System.err
    System.setErr(new PrintStream(new ByteArrayOutputStream()))
    body
    System.setErr(temp)
  }

  def getClassName(a: Any) = a.getClass.toString.substring(a.getClass.toString.lastIndexOf('.') + 1)

  def filterByType[T](list: List[Any])(implicit mf: Manifest[T]): List[T] = {
    list.collect({
      case t if mf.erasure.isAssignableFrom(t.getClass) => t
    }).asInstanceOf[List[T]]
  }

  def convert[To](from: Any): Option[To] = if (from.isInstanceOf[To]) Some(from.asInstanceOf[To]) else None

  //TODO: use currying if applicable
  //TODO: getAnnotationsWithin(start: Int, end: Int)(descendants:Iterable[Span])
  //TODO: getAnnotationsStartingAt(start: Int)
  //TODO: getAnnotationsStartingAfter(start: Int)
  //TODO: getAnnotationsEndingAt(end: Int)
  //TODO: getAnnotationsEndingBefore(end: Int)
  //TODO: getAnnotationsWithin(s: Span) = getAnnotationsWithin(s.start, s.end)

  def deepCopy[A](a: A)(implicit m: reflect.Manifest[A]): A = util.Marshal.load[A](util.Marshal.dump(a))
}