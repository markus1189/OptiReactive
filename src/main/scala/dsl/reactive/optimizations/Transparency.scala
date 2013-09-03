package dsl.reactive.optimizations

import scala.virtualization.lms.common.{Base, LiftVariables}
import dsl.reactive.syntaxops.VarSyntax

trait TransparentReactivity extends Base {
  this: VarSyntax with LiftVariables =>

  def __newVar[T:Manifest](value: T): Rep[dsl.reactive.Var[T]] = new_reactive_var(unit(value))
  def __assign[T:Manifest](lhs: Rep[dsl.reactive.Var[T]], rhs: Rep[T]): Rep[Unit] = dep_holder_set(lhs,rhs)

}
