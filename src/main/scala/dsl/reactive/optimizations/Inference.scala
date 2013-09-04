package dsl.reactive.optimizations

import scala.virtualization.lms.common.{Base, EffectExp}
import dsl.reactive.syntaxops.{SignalSyntax, VarSyntax, DepHolderOps}
import dsl.reactive.phantom._

/** Adds another Signal factory method, ISignal {}, that automatically infers
    the dependencies, given an expression
  */
trait InferredSignals extends Base {
  this: SignalSyntax =>

  object ISignal {
    def apply[A:Manifest](f: => Rep[A]) =
      new_inferred_signal(f)
  }

  def new_inferred_signal[A:Manifest](f: => Rep[A]): Rep[Behavior[A]]
}

trait InferredSignalsExp extends InferredSignals with EffectExp {
  this: SignalSyntax with DepHolderOps =>

  override def new_inferred_signal[A:Manifest](f: => Exp[A]): Exp[Behavior[A]] =
    new_behavior(inferReactiveAccess(reifyEffects(f)), f)

  /* Check the effectSyms of the Block for AccessDepHolder IR nodes */
  def inferReactiveAccess(bdy: Block[_]): List[Exp[DepHolder]] = {
      val effects = effectSyms(bdy)
      val onlySyms = effects.filter { case Sym(x) => true; case _ => false }
      val defs = onlySyms.map(findDefinition(_)).collect {
        case Some(TP(_,Reflect(AccessDepHolder(access),_,_))) => access
      }

    defs.asInstanceOf[List[Exp[DepHolder]]].distinct
  }
}