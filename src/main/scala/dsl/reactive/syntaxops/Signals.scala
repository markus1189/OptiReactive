package dsl.reactive.syntaxops

import scala.virtualization.lms.common.{Base, EffectExp}
import dsl.reactive.PhantomTypes._

trait SignalSyntax extends Base {
  object Signal {
    def apply[A:Manifest](dhs: Rep[DepHolder]*)(f: => Rep[A]) =
      new_behavior(dhs, f)
  }

  def new_behavior[A:Manifest](
    dhs: Seq[Rep[DepHolder]], f: => Rep[A]): Rep[Behavior[A]]
}

trait SignalOps extends EffectExp {
  this: SignalSyntax =>

  case class SignalCreation[A:Manifest](
    dhs: Seq[Exp[DepHolder]],
    body: Block[A]
  ) extends Def[Behavior[A]]

  override def new_behavior[A:Manifest](
    dhs: Seq[Exp[DepHolder]],
    f: => Exp[A]
  ): Exp[Behavior[A]] = SignalCreation(dhs, reifyEffects(f))

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case SignalCreation(dhs,body) => effectSyms(body)
    case _ => super.boundSyms(e)
  }
}