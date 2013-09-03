package dsl.reactive.syntaxops

import scala.virtualization.lms.common.{Base,EffectExp}
import dsl.reactive.PhantomTypes._

trait ReactiveEntitySyntax extends Base {

  implicit def toReactiveEntityOps(entity: Rep[ReactiveEntity]) =
    new ReactiveEntityOps(entity)

  class ReactiveEntityOps(entity: Rep[ReactiveEntity]) {
    def getDependentsList: Rep[List[ReactiveEntity]] =
      reactive_entity_dependents_list(entity)

    def reEvaluate() = re_evaluate(entity)
  }

  def reactive_entity_dependents_list(
    entity: Rep[ReactiveEntity]): Rep[List[ReactiveEntity]]

  def re_evaluate(elem: Rep[ReactiveEntity]): Rep[Unit]
}

trait ReactiveEntityOps extends EffectExp {
  this: ReactiveEntitySyntax =>

  case class GetDependentsList(
    dh: Exp[ReactiveEntity]) extends Def[List[ReactiveEntity]]

  override def reactive_entity_dependents_list(
    entity: Exp[ReactiveEntity]): Exp[List[ReactiveEntity]] =
    GetDependentsList(entity)

  case class ReEvaluation(elem: Exp[ReactiveEntity]) extends Def[Unit]

  override def re_evaluate(elem: Exp[ReactiveEntity]): Exp[Unit] = {
    reflectEffect(ReEvaluation(elem))
  }

}