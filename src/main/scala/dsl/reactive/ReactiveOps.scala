package dsl.reactive

import scala.virtualization.lms.common._

import dsl.reactive.syntaxops._
import dsl.reactive.optimizations._
import dsl.reactive.generalpurpose._

import dsl.reactive.phantom._

/** This trait defines the accepted syntactic constructs for programs
  * written in the DSL
  */
trait Reactivity
    extends MeasureOps
    with ExpensiveOps
    with VarSyntax
    with SignalSyntax
    with DepHolderSyntax
    with ReactiveEntitySyntax
    with InferredSignals
    with PointSyntax
    with RangeOps

/** This trait has to provide the concrete implementations for the
  * syntactically allowed constructs of the `Reactivity` trait
  */
trait ReactivityExp extends Reactivity
                    with MeasureOpsExp
                    with ExpensiveOpsExp
                    with ListOpsExp
                    with SeqOpsExp
                    with EffectExp
                    with InferredSignalsExp
                    with VarOps
                    with SignalOps
                    with DepHolderOps
                    with ReactiveEntityOps
                    with PointExp
                    with RangeOpsExp

// Optimize Signals with constant dependencies
trait ReactivityExpOpt extends ReactivityExp {
  private def onlyConstants(dhs: Seq[Exp[DepHolder]]): Boolean =  {
    val syms: Seq[Sym[DepHolder]] = dhs.filter {
      case Sym(x) => true
      case _ => false
    }.asInstanceOf[Seq[Sym[DepHolder]]]

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

trait ScalaGenReactivity extends ScalaGenBase with ScalaGenEffect {
  val IR: ReactivityExp
  import IR._

  def dsmap(s: String) = s.replaceAll("dsl.reactive.phantom", "dsl.reactive.simplereactive")

  override def remap[A](m: Manifest[A]): String = dsmap(super.remap(m))

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit =  node match {
    case AccessDepHolder(dh)    => emitValDef(sym, quote(dh) + ".get")
    case ReEvaluation(elem)     => emitValDef(sym, quote(elem) + ".forceReEval()")
    case GetDependentsList(dh)  => emitValDef(sym, quote(dh) + ".getDependentsList")
    case SetDepHolder(dh,value) => emitValDef(sym, quote(dh) + ".set(" + quote(value) + ")")
    case VarCreation(v)         => emitValDef(sym, "dsl.reactive.simplereactive.ReactiveVar(" + quote(v) + ")")
    case SignalCreation(dhs,f)  => emitValDef(sym, "dsl.reactive.simplereactive.Signal(" + dhs.map(quote).mkString(", ") + ") { ")
                                     emitBlock(f)
                                     stream.println(quote(getBlockResult(f)) + "\n")
                                   stream.println("}")
    case _                      => super.emitNode(sym,node)
  }
}

trait ScalaGenReactivityOpt extends ScalaGenReactivity {
  val IR: ReactivityExpOpt
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit =  node match {
    case ConstantAccess(f) => emitValDef(sym, quote(getBlockResult(f)))
    case ConstantCreation(f) => emitValDef(sym, "dsl.reactive.simplereactive.Constant {")
                                  emitBlock(f)
                                  stream.println(quote(getBlockResult(f)) + "\n")
                                stream.println("}")
    case _ => super.emitNode(sym,node)

  }
}