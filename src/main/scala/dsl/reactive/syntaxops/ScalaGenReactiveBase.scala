package dsl.reactive.syntaxops

import scala.virtualization.lms.common.{ScalaGenBase}

/** Code generator base trait.
  * The main purpose of this trait is to map the phantom types to simplereactive.
  * By changing this trait and the code generators, it should be easy to change from
  * SimpleReactive to another framework
  */
trait ScalaGenReactiveBase extends ScalaGenBase {
  val simpleReactivePkg = "dsl.reactive.simplereactive."

  def dsmap(s: String) =
    s.replaceAll("dsl.reactive.phantom", "dsl.reactive.simplereactive")

  override def remap[A](m: Manifest[A]): String = dsmap(super.remap(m))
}