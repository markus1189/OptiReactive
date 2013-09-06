package dsl.reactive.optimizations

import scala.virtualization.lms.common.{Base, EffectExp, ScalaGenEffect, FunctionsExp}
import scala.virtualization.lms.internal.Expressions
import dsl.reactive.syntaxops.{SignalOps, SignalSyntax, ScalaGenReactiveBase}
import dsl.reactive.phantom._
import language.implicitConversions

trait FusedMappings extends Base {
  implicit def toFusedMapping[A:Manifest](b: Rep[Behavior[A]]) = new FusedMappingsOps(b)
  case class FusedMappingsOps[A:Manifest](b: Rep[Behavior[A]]) {
    def fuseMap[B:Manifest](f: Rep[A] => Rep[B]): Rep[Behavior[B]] = fused_mapping(b,f)
  }

  def fused_mapping[A:Manifest,B:Manifest](
    b: Rep[Behavior[A]],
    f: Rep[A] => Rep[B]
  ): Rep[Behavior[B]]
}

trait FusedMappingsOps extends FusedMappings with FunctionsExp {
  this: SignalOps =>

  case class FunctionComposition[A:Manifest,B:Manifest,C:Manifest](
    f: Exp[A => B],
    g: Exp[B => C]
  ) extends Def[A => C]

  def infix_compose[A:Manifest,B:Manifest,C:Manifest](
    f: Exp[B => C], g: Exp[A => B]): Exp[A => C] =
    FunctionComposition(g,f)

  case class FuseMappedBehavior[A:Manifest,B:Manifest](
    sig: Exp[Behavior[A]],
    f: Rep[A => B]
  ) extends Def[Behavior[B]]

  override def fused_mapping[A:Manifest,B:Manifest](sig: Exp[Behavior[A]],
    f: Exp[A] => Exp[B]): Exp[Behavior[B]] = sig match {
    case Def(FuseMappedBehavior(a,b)) => FuseMappedBehavior(a,doLambda(f)compose b)
    case _ => FuseMappedBehavior(sig,doLambda(f))
  }
}

trait ScalaGenFusedMapping extends ScalaGenReactiveBase {
  val IR: FusedMappingsOps
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit =  node match {
    case FunctionComposition(g,f) => emitValDef(sym,
      quote(f) + ".compose(" + quote(g) +")")
    case FuseMappedBehavior(s,f) => emitValDef(sym, quote(s) + ".map(" + quote(f) + ")")
    case _ => super.emitNode(sym,node)
  }
}