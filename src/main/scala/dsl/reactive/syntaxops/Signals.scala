package dsl.reactive.syntaxops

import scala.virtualization.lms.common.{Base, EffectExp, ScalaGenFunctions, FunctionsExp}
import dsl.reactive.phantom._
import language.implicitConversions

/** Defines Signal syntax, methods and code generators. */
trait SignalSyntax extends Base {

  /* Enabling signal.map(_ + 1) via implicit class*/
  implicit def toBehaviorOps[A:Manifest](s: Rep[Behavior[A]]) = new BehaviorOps(s)
  class BehaviorOps[A:Manifest](s: Rep[Behavior[A]]) {
    def map(f: Rep[A] => Rep[A]) = mapping_behavior(s,f)
  }

  def mapping_behavior[A:Manifest](sig: Rep[Behavior[A]],
    f: Rep[A] => Rep[A]): Rep[Behavior[A]]

  object Signal {
    /* The Signal expression factory method */
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

  case class MappedBehavior[A:Manifest](
    sig: Exp[Behavior[A]],
    f: Rep[A => A]
  ) extends Def[Behavior[A]]

  override def mapping_behavior[A:Manifest](sig: Exp[Behavior[A]],
    f: Exp[A] => Exp[A]): Exp[Behavior[A]] = MappedBehavior(sig,doLambda(f))


  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case SignalCreation(dhs,body) => effectSyms(body)
    case _ => super.boundSyms(e)
  }
}

trait ScalaGenSignals extends ScalaGenReactiveBase with ScalaGenFunctions {
  val IR: SignalOps
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit =  node match {
    /* emit the stored block at the inside of the Signal expression */
    case SignalCreation(dhs,f)  => emitValDef(sym,
      simpleReactivePkg + "Signal(" + dhs.map(quote).mkString(", ") + ") { ")
        emitBlock(f)
        stream.println(quote(getBlockResult(f)) + "\n")
      stream.println("}")
    /* mapping is provided by the underlying framework */
    case MappedBehavior(s,f) => emitValDef(sym, quote(s) + ".map(" + quote(f) + ")")
    case _ => super.emitNode(sym,node)
  }
}