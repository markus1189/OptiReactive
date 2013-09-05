package dsl.reactive.optimizations

import scala.virtualization.lms.common.{Base, EffectExp, ScalaGenEffect, FunctionsExp}
import scala.virtualization.lms.internal.Expressions
import dsl.reactive.syntaxops.{SignalOps, SignalSyntax, ScalaGenReactiveBase}
import dsl.reactive.phantom._
import language.implicitConversions

trait FusedMappings extends Base {
  def infix_fuseMap[A:Manifest](
    b: Rep[Behavior[A]],
    f: Rep[A] => Rep[A]
  ) = fused_mapping(b,f)

  implicit def toFusedMapping[A:Manifest](b: Rep[Behavior[A]]) = new FusedMappingsOps(b)
  case class FusedMappingsOps[A:Manifest](b: Rep[Behavior[A]]) {
    def fuseMap(f: Rep[A] => Rep[A]): Rep[Behavior[A]] = fused_mapping(b,f)
  }

  def fused_mapping[A:Manifest](
    b: Rep[Behavior[A]],
    f: Rep[A] => Rep[A]
  ): Rep[Behavior[A]]
}

trait FusedMappingsOps extends FusedMappings with FunctionsExp {
  this: SignalOps =>

  case class FunctionComposition[A:Manifest](
    f: Exp[A => A],
    g: Exp[A => A]
  ) extends Def[A => A]

  def infix_compose[A:Manifest](f: Exp[A => A], g: Exp[A => A]): Exp[A => A] =
    FunctionComposition(f,g)

  case class FuseMappedBehavior[A:Manifest](
    sig: Exp[Behavior[A]],
    f: Rep[A => A]
  ) extends Def[Behavior[A]]

  override def fused_mapping[A:Manifest](sig: Exp[Behavior[A]],
    f: Exp[A] => Exp[A]): Exp[Behavior[A]] = sig match {
    case Def(FuseMappedBehavior(a,b)) => {
      val bNew = b.asInstanceOf[Exp[A => A]]
      FuseMappedBehavior(a,doLambda(f)compose bNew)
    }
    case _ => FuseMappedBehavior(sig,doLambda(f))
  }
}

trait ScalaGenFusedMapping extends ScalaGenReactiveBase {
  val IR: FusedMappingsOps
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit =  node match {
    case FunctionComposition(f,g) => emitValDef(sym,
      quote(f) + ".compose(" + quote(g) +")")
    case FuseMappedBehavior(s,f) => emitValDef(sym, quote(s) + ".map(" + quote(f) + ")")
    case _ => super.emitNode(sym,node)
  }
}