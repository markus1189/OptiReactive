package dsl.reactive.syntaxops

import scala.virtualization.lms.common.{Base, EffectExp, ScalaGenFunctions, FunctionsExp}
import dsl.reactive.phantom._
import language.implicitConversions

trait SignalSyntax extends Base {

  implicit def toBehaviorOps[A:Manifest](s: Rep[Behavior[A]]) = new BehaviorOps(s)

  class BehaviorOps[A:Manifest](s: Rep[Behavior[A]]) {
    def map[B:Manifest](f: Rep[A] => Rep[B]) = mapping_behavior(s,f)
  }

  def mapping_behavior[A:Manifest,B:Manifest](sig: Rep[Behavior[A]],
    f: Rep[A] => Rep[B]): Rep[Behavior[B]]

  object Signal {
    def apply[A:Manifest](dhs: Rep[DepHolder]*)(f: => Rep[A]) =
      new_behavior(dhs, f)
  }

  def new_behavior[A:Manifest](
    dhs: Seq[Rep[DepHolder]], f: => Rep[A]): Rep[Behavior[A]]
}

trait SignalOps extends EffectExp with FunctionsExp {
  this: SignalSyntax =>

  case class SignalCreation[A:Manifest](
    dhs: Seq[Exp[DepHolder]],
    body: Block[A]
  ) extends Def[Behavior[A]]

  override def new_behavior[A:Manifest](
    dhs: Seq[Exp[DepHolder]],
    f: => Exp[A]
  ): Exp[Behavior[A]] = SignalCreation(dhs, reifyEffects(f))

  case class MappedBehavior[A:Manifest,B:Manifest](
    sig: Exp[Behavior[A]],
    f: Rep[A => B]
  ) extends Def[Behavior[B]]

  override def mapping_behavior[A:Manifest,B:Manifest](sig: Exp[Behavior[A]],
    f: Exp[A] => Exp[B]): Exp[Behavior[B]] = MappedBehavior(sig,doLambda(f))


  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case SignalCreation(dhs,body) => effectSyms(body)
    case _ => super.boundSyms(e)
  }
}

trait ScalaGenSignals extends ScalaGenReactiveBase with ScalaGenFunctions {
  val IR: SignalOps
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit =  node match {
    case SignalCreation(dhs,f)  => emitValDef(sym,
      simpleReactivePkg + "Signal(" + dhs.map(quote).mkString(", ") + ") { ")
        emitBlock(f)
        stream.println(quote(getBlockResult(f)) + "\n")
      stream.println("}")
    case MappedBehavior(s,f) => emitValDef(sym, quote(s) + ".map(" + quote(f) + ")")
    case _ => super.emitNode(sym,node)
  }
}