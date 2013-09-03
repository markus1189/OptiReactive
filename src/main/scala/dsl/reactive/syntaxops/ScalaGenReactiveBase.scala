package dsl.reactive.syntaxops

import scala.virtualization.lms.common.{ScalaGenBase}

trait ScalaGenReactiveBase extends ScalaGenBase {
  val simpleReactivePkg = "dsl.reactive.simplereactive."

  def dsmap(s: String) =
    s.replaceAll("dsl.reactive.phantom", "dsl.reactive.simplereactive")

  override def remap[A](m: Manifest[A]): String = dsmap(super.remap(m))
}