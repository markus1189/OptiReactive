package dsl.reactive.syntaxops

import language.implicitConversions
import scala.virtualization.lms.common.{Base, EffectExp}
import dsl.reactive.phantom._

/** Defines syntax, methods and generators for ReactiveVars
  * Note that Var instead of ReactiveVar as a name does not work out
  * very well, because LMS has it's own Var type
  */
trait VarSyntax extends Base {
  implicit def toVarOps[A:Manifest](v: Rep[ReactiveVar[A]]): VarOps[A] = new VarOps(v)
  class VarOps[A:Manifest](v: Rep[ReactiveVar[A]]) {
    def set(value: Rep[A]): Rep[Unit] = dep_holder_set(v, value)
  }

  def dep_holder_set[A:Manifest](v: Rep[ReactiveVar[A]], value: Rep[A]): Rep[Unit]

  object ReactiveVar {
    def apply[A:Manifest](v: Rep[A]): Rep[ReactiveVar[A]] = new_reactive_var(v)
  }

  def new_reactive_var[A:Manifest](v: Rep[A]): Rep[ReactiveVar[A]]
}

trait VarOps extends EffectExp {
  this: VarSyntax =>

  case class VarCreation[A:Manifest](
    value: Exp[A]) extends Def[ReactiveVar[A]]

  override def new_reactive_var[A:Manifest](
    v: Exp[A]): Exp[ReactiveVar[A]] = VarCreation(v)

  case class SetDepHolder[A:Manifest](
    dh: Exp[ReactiveVar[A]],
    value: Exp[A]) extends Def[Unit]

  override def dep_holder_set[A:Manifest](
    dh: Exp[ReactiveVar[A]],
    value: Exp[A]): Exp[Unit] = reflectEffect(SetDepHolder(dh,value))
}

trait ScalaGenVars extends ScalaGenReactiveBase {
  val IR: VarOps
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {
    case VarCreation(v) => emitValDef(sym,
      simpleReactivePkg + "ReactiveVar(" + quote(v) + ")")

    case SetDepHolder(dh,value) => emitValDef(sym,
      quote(dh) + ".set(" + quote(value) + ")")
    case _ => super.emitNode(sym,node)
  }
}