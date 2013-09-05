package dsl.reactive.optimizations

import scala.virtualization.lms.common.{Base, EffectExp, ScalaGenEffect}
import scala.virtualization.lms.internal.Expressions
import dsl.reactive.syntaxops.{SignalOps, SignalSyntax, ScalaGenReactiveBase}
import dsl.reactive.phantom._

trait FusedMapping extends SignalSyntax with SignalOps {
  case class FunctionComposition[A:Manifest](
    f: Exp[A => A],
    g: Exp[A => A]
  ) extends Def[A => A]

  def infix_compose[A:Manifest](f: Exp[A => A], g: Exp[A => A]): Exp[A => A] =
    FunctionComposition(f,g)

  override def mapping_behavior[A:Manifest](sig: Exp[Behavior[A]],
    f: Exp[A] => Exp[A]): Exp[Behavior[A]] = sig match {
    case Def(MappedBehavior(a,b)) => {
      val bNew = b.asInstanceOf[Exp[A => A]]
      MappedBehavior(a,doLambda(f)compose bNew)
    }
    case _ => MappedBehavior(sig,doLambda(f))
  }

}

trait ScalaGenFusedMapping extends ScalaGenReactiveBase {
  val IR: FusedMapping
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit =  node match {
    case FunctionComposition(f,g) => emitValDef(sym,
      quote(f) + ".compose(" + quote(g) +")")
    case _ => super.emitNode(sym,node)
  }
}