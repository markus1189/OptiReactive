package dsl.reactive.optimizations

import scala.virtualization.lms.common.EffectExp

import dsl.reactive.syntaxops.{SignalSyntax, SignalOps, DepHolderSyntax, DepHolderOps}
import dsl.reactive.phantom._

trait ConstantElimination extends EffectExp
    with SignalSyntax
    with SignalOps
    with DepHolderSyntax
    with DepHolderOps {

  private def onlyConstants(dhs: Seq[Exp[DepHolder]]): Boolean =  {
    val syms: Seq[Sym[DepHolder]] = dhs.filter {
      case Sym(x) => true
      case _ => false }.asInstanceOf[Seq[Sym[DepHolder]]]

    val defs: Seq[Def[Any]] = syms.map(findDefinition(_)).map {
      case Some(TP(_,rhs)) => Some(rhs)
      case _ => None
    }.filter(_.isDefined).map(_.get)

    defs.forall {
      case ConstantCreation(_) => true
      case _ => false
    }
  }

  override def new_behavior[A:Manifest](
    dhs: Seq[Exp[DepHolder]], f: => Exp[A]): Exp[Behavior[A]] = {

    if (dhs.isEmpty || onlyConstants(dhs)) {
      // Signal creation without dependency holders => constant
      // Signal with only constant deps => constant
      ConstantCreation(reifyEffects(f))
    } else {
      SignalCreation(dhs, reifyEffects(f))
    }
  }

  case class ConstantCreation[A:Manifest]( body: Block[A]) extends Def[Behavior[A]]
  case class ConstantAccess[A:Manifest](body : Block[A]) extends Def[A]

  override def dep_holder_access[A:Manifest](dh: Exp[AccessableDepHolder[A]]): Exp[A] = dh match {
    case Def(ConstantCreation(x)) => ConstantAccess(x)
    case _ => super.dep_holder_access(dh)
  }

}