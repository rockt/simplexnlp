package simplexnlp.core

object Implicits {
  implicit def componentToPipeline(component: Component): Pipeline = new Pipeline(component)
  implicit def intToString(int:Int): String = int.toString
}