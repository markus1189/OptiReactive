package dsl.reactive.syntaxops

import language.implicitConversions
import scala.virtualization.lms.common.{Base,EffectExp}
import dsl.reactive.phantom._

/** This file defines the methods specific to ReactiveEntity, namely
  * getDependentsList and forceReEval
  */

trait ReactiveEntitySyntax extends Base {

  implicit def toReactiveEntityOps(entity: Rep[ReactiveEntity]) =
    new ReactiveEntityOps(entity)

  class ReactiveEntityOps(entity: Rep[ReactiveEntity]) {
    def getDependentsList: Rep[List[ReactiveEntity]] =
      reactive_entity_dependents_list(entity)

    def forceReEval() = re_evaluate(entity)
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

trait ScalaGenReactiveEntities extends ScalaGenReactiveBase {
  val IR: ReactiveEntityOps
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit =  node match {
    case ReEvaluation(elem) => emitValDef(sym, quote(elem) + ".forceReEval()")
    case GetDependentsList(dh) => emitValDef(sym, quote(dh) + ".getDependentsList")
    case _ => super.emitNode(sym,node)
  }
}