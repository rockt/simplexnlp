package simplexnlp

import core.{Pipeline, Component}

object Implicits {
  implicit def componentToPipeline(component:Component):Pipeline = new Pipeline(component)
}