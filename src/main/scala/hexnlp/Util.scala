package hexnlp

import java.io.{ByteArrayOutputStream, PrintStream}

object Util {
  def suppressConsoleOutput(body: => Any) = {
    val temp = System.err
    System.setErr(new PrintStream(new ByteArrayOutputStream()))
    body
    System.setErr(temp)
  }
  def getClassName(a:Any) = a.getClass.toString.substring(a.getClass.toString.lastIndexOf('.') + 1)
  def filterByType[T](list:List[Any])(implicit mf:Manifest[T]):List[T] = {
    list.collect({
      case t if mf.erasure.isAssignableFrom(t.getClass) => t
    }).asInstanceOf[List[T]]
  }
  //TODO: use currying if applicable
  //TODO: getAnnotationsWithin(start: Int, end: Int)(descendants:Iterable[Span])
  //TODO: getAnnotationsStartingAt(start: Int)
  //TODO: getAnnotationsStartingAfter(start: Int)
  //TODO: getAnnotationsEndingAt(end: Int)
  //TODO: getAnnotationsEndingBefore(end: Int)
  //TODO: getAnnotationsWithin(s: Span) = getAnnotationsWithin(s.start, s.end)
}