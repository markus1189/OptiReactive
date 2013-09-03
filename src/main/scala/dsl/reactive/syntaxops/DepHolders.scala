package dsl.reactive.syntaxops
import language.implicitConversions
import scala.virtualization.lms.common.Base
import scala.virtualization.lms.common.EffectExp
import dsl.reactive.phantom._

trait DepHolderSyntax extends Base {

  implicit def toAccessableDepHolderOps[A:Manifest](
    dh: Rep[AccessableDepHolder[A]]) =
      new AccessableDepHolderOps(dh)

  class AccessableDepHolderOps[A:Manifest](dh: Rep[AccessableDepHolder[A]]) {
    def get: Rep[A] = dep_holder_access(dh)
  }

  def dep_holder_access[A:Manifest](dh: Rep[AccessableDepHolder[A]]): Rep[A]
}


trait DepHolderOps extends EffectExp {
  this: DepHolderSyntax =>

  case class AccessDepHolder[A:Manifest](
    dh: Exp[AccessableDepHolder[A]]) extends Def[A]

  override def dep_holder_access[A:Manifest](
    dh: Exp[AccessableDepHolder[A]]): Exp[A] =
    reflectMutable(AccessDepHolder(dh))
}