package dsl.reactive

import scala.virtualization.lms.common._

trait ExpensiveOps extends Base {
  def expensive(x: Rep[Long]): Rep[Long]
}

trait ExpensiveOpsExp extends ExpensiveOps with EffectExp {
  case class Expensive(x: Exp[Long]) extends Def[Long]
  def expensive(x: Exp[Long]): Exp[Long] = {
    new Expensive(x)
  }
}

trait ScalaGenExpensiveOps extends ScalaGenBase {
  val IR: ExpensiveOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit =  node match {
    case Expensive(x) => emitValDef(sym,
      "{ def fib(x: Long): Long = x match { case x if x < 2 => 1; case _ => fib(x-1) + fib(x-2) }" + "\n fib(" + quote(x) + ") }")
      //"{ def fac(x: Long): Long = if (x <= 1) 1 else { x*fac(x-1) }; \n" +
      //"fac(" + quote(x) + ") }")
    case _         => super.emitNode(sym,node)
  }
}