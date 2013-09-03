package dsl.reactive.generalpurpose

import scala.virtualization.lms.common.{Base,EffectExp,ScalaGenEffect,ScalaGenBase}
import dsl.reactive.Point

trait PointSyntax extends Base {
  object Point {
    def apply(x: Rep[Int], y: Rep[Int]): Rep[Point] = new_point(x,y)
  }

  def new_point(x: Rep[Int], y: Rep[Int]): Rep[Point]

  def infix_x(p: Rep[Point]): Rep[Int]
  def infix_y(p: Rep[Point]): Rep[Int]

  def euclidDistance(p1: Rep[Point], p2: Rep[Point]): Rep[Double]
}

trait PointExp extends PointSyntax with EffectExp {

  case class CreatePoint(x: Exp[Int], y: Exp[Int]) extends Def[Point]
  override def new_point(x: Exp[Int], y: Exp[Int]) = new CreatePoint(x,y)

  case class EuclideanDistance(p1: Exp[Point], p2: Exp[Point]) extends Def[Double]
  override def euclidDistance(p1: Exp[Point], p2: Exp[Point]): Exp[Double] = new EuclideanDistance(p1,p2)

  case class XAccess(p: Exp[Point]) extends Def[Int]
  case class YAccess(p: Exp[Point]) extends Def[Int]
  def infix_x(p: Exp[Point]): Exp[Int] = XAccess(p)
  def infix_y(p: Exp[Point]): Exp[Int] = YAccess(p)
}

trait ScalaGenPoint extends ScalaGenBase with ScalaGenEffect {
  val IR: PointExp
  import IR._


  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case XAccess(p) => emitValDef(sym, quote(p) + ".x")
    case YAccess(p) => emitValDef(sym, quote(p) + ".y")
    case CreatePoint(x,y) => emitValDef(sym,"Point(" + quote(x) + "," + quote(y) + ")")
    case EuclideanDistance(p1,p2) => emitValDef(sym,
      "math.sqrt(" +
        "math.pow(" + quote(p1) + ".x - " + quote(p2) + ".x, 2) " +
        "+" +
        "math.pow(" + quote(p1) + ".y - " + quote(p2) + ".y, 2) " +
      ")"
    )
    case _ => super.emitNode(sym,rhs)
  }
}