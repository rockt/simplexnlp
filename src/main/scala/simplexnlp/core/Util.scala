package simplexnlp.core

import java.io.{ByteArrayOutputStream, PrintStream}
import java.util.Random

object Util {
  def random = new Random(1337)

  def round(d: Double)(p: Int): Double = {
    val t = math pow (10, p)
    (math rint d * t) / t
  }

  val enableSuppressing = true //FIXME: use Config parameter instead
  def suppressConsoleOutput(body: => Any) =
    if (enableSuppressing) {
      val tempErr = System.err
      val tempOut = System.out
      val nirvana = new PrintStream(new ByteArrayOutputStream())
      System.setErr(nirvana)
      System.setOut(nirvana)
      body
      System.setErr(tempErr)
      System.setOut(tempOut)
    } else body

  def suppressLoggerOutput() =
    if (enableSuppressing) {
      import java.util.logging._
      LogManager.getLogManager.reset()
      val globalLogger = Logger.getLogger(java.util.logging.Logger.GLOBAL_LOGGER_NAME)
      globalLogger.setLevel(java.util.logging.Level.OFF)
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

  class Timer {
    def current = System.currentTimeMillis
    private val start0 = current
    private var start = start0
    def diff(time: Long) = current - time
    def diff = current - start
    def stop = {
      val temp = diff
      start = current
      temp
    }
    //TODO: should find best formatting on his own
    def report(): String = report("s")
    def report(format: String): String = report(diff, format)
    def reportStop(format: String): String = report(stop, format)
    def reportTotal(format: String): String = report(total, format)
    def report(ms: Long): String = report(ms, "%1.0fms")
    def report(ms: Long, format: String): String = {
      if (format.matches("ms|s|m|h|d|w")) report(ms, "%1.0f" + format)
      else {
        def report0(time: Double, format: String): Double = format match {
          case "ms" => time
          case "s" => report0(time/1000, "ms")
          case "m" => report0(time/60, "s")
          case "h" => report0(time/60, "m")
          case "d" => report0(time/24, "h")
          case "w" => report0(time/7, "d")
          case _ => report0(time, "ms")
        }
        format.format(report0(ms, format.takeRight(if(format.endsWith("ms")) 2 else 1)))
      }
    }
    def reset() { start = current }
    def total = diff(start0)
  }
}