package hexnlp

import java.io.{ByteArrayOutputStream, PrintStream}

object Util {
  def suppressConsoleOutput(body: => Any) = {
    val temp = System.err
    System.setErr(new PrintStream(new ByteArrayOutputStream()))
    body
    System.setErr(temp)
  }
}