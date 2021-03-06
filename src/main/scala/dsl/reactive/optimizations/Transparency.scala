package dsl.reactive.optimizations

import scala.virtualization.lms.common.{Base, LiftVariables}
import dsl.reactive.syntaxops.VarSyntax
import dsl.reactive._
import dsl.reactive.phantom._

/** This trait overloads the variable creation and assignment methods, to
  * to use OptiReactive's ReactiveVars.
  * The methods are provided by Scala-Virtualized and the LiftVariables trait
  */
trait TransparentReactivity extends Base {
  this: VarSyntax with LiftVariables =>

  def __newVar[T:Manifest](value: T): Rep[ReactiveVar[T]] = new_reactive_var(unit(value))
  def __assign[T:Manifest](lhs: Rep[ReactiveVar[T]], rhs: Rep[T]): Rep[Unit] = dep_holder_set(lhs,rhs)

}