package simplexnlp

object Implicits {
  implicit def componentToPipeline(component:Component):Pipeline = new Pipeline(component)
}