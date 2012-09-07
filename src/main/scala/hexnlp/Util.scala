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

  //TODO: use currying if applicable
  //TODO: getAnnotationsWithin(start: Int, end: Int)(annotations:Iterable[Span])
  //TODO: getAnnotationsStartingAt(start: Int)
  //TODO: getAnnotationsStartingAfter(start: Int)
  //TODO: getAnnotationsEndingAt(end: Int)
  //TODO: getAnnotationsEndingBefore(end: Int)
  //TODO: getAnnotationsWithin(s: Span) = getAnnotationsWithin(s.start, s.end)
}