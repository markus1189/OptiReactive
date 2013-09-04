package dsl.reactive.optimizations

import scala.virtualization.lms.common.{Base, EffectExp, ScalaGenEffect}
import dsl.reactive.syntaxops.{SignalOps, DepHolderOps, DepHolderSyntax, ScalaGenReactiveBase}
import dsl.reactive.phantom._

/** A Constant is a reactive (Signal/Behavior), that does either:
  * - depend on nothing
  * - depend only on other constants
  *
  * in both cases, we can replace them with a special constant type
  */
trait ConstantFolding extends EffectExp with DepHolderSyntax with DepHolderOps {
  self: SignalOps =>

  /* Check if all Expressions of the Seq are constants */
  private def onlyConstants(dhs: Seq[Exp[DepHolder]]): Boolean = {
    def filterForSyms[A](in: Seq[Exp[A]]): Seq[Sym[A]] = in.filter {
      case Sym(x) => true
      case _ => false
    }.asInstanceOf[Seq[Sym[A]]] // this is safe due to the pattern match

    def retrieveDefinition[A](in: Seq[Sym[A]]): Seq[Def[Any]] = {
      in.map(findDefinition(_)).map {
        case Some(TP(_,rhs)) => Some(rhs)
        case _ => None
      }.filter(_.isDefined).map(_.get)
    }

    val syms = filterForSyms(dhs)
    val defs = retrieveDefinition(syms)

    defs.forall {
      case ConstantCreation(_) => true
      case _ => false
    }
  }

  /* Override the behavior factory method, to create Constants instead of Signals,
   * if possible
   */
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

trait ScalaGenConstantFolding extends ScalaGenReactiveBase with ScalaGenEffect {
  val IR: ConstantFolding
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit =  node match {
    case ConstantAccess(f) => emitValDef(sym, quote(getBlockResult(f)))
    /* Unfold the stored block inside of a Constant expression */
    case ConstantCreation(f) => emitValDef(sym,
      simpleReactivePkg + "Constant {")
        emitBlock(f)
        stream.println(quote(getBlockResult(f)) + "\n")
      stream.println("}")
    case _ => super.emitNode(sym,node)
  }
}