package dsl.reactive.syntaxops

import scala.virtualization.lms.common.{Base, EffectExp}
import dsl.reactive.Var

trait VarSyntax extends Base {
  implicit def toVarOps[A:Manifest](v: Rep[Var[A]]): VarOps[A] = new VarOps(v)
  class VarOps[A:Manifest](v: Rep[Var[A]]) {
    def set(value: Rep[A]): Rep[Unit] = dep_holder_set(v, value)
  }

  def dep_holder_set[A:Manifest](v: Rep[Var[A]], value: Rep[A]): Rep[Unit]

  object Var {
    def apply[A:Manifest](v: Rep[A]): Rep[Var[A]] = new_reactive_var(v)
  }

  def new_reactive_var[A:Manifest](v: Rep[A]): Rep[Var[A]]
}

trait VarOps extends EffectExp {
  this: VarSyntax =>

  case class VarCreation[A:Manifest](
    value: Exp[A]) extends Def[Var[A]]

  override def new_reactive_var[A:Manifest](
    v: Exp[A]): Exp[Var[A]] = VarCreation(v)

  case class SetDepHolder[A:Manifest](
    dh: Exp[dsl.reactive.Var[A]],
    value: Exp[A]) extends Def[Unit]

  override def dep_holder_set[A:Manifest](
    dh: Exp[dsl.reactive.Var[A]],
    value: Exp[A]): Exp[Unit] = reflectEffect(SetDepHolder(dh,value))

}
