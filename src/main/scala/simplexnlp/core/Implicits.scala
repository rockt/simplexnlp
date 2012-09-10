package simplexnlp.core


object Implicits {
  implicit def componentToPipeline(component: Component): Pipeline = new Pipeline(component)
}