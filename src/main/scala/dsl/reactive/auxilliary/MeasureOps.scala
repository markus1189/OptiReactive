package dsl.reactive

import scala.virtualization.lms.common._

trait MeasureOps extends Base {
  def printTime(): Rep[Unit]
}

trait MeasureOpsExp extends MeasureOps with EffectExp {
  case object PrintTime extends Def[Unit]
  def printTime() = reflectEffect(PrintTime)
}

trait ScalaGenMeasureOps extends ScalaGenBase {
  val IR: MeasureOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit =  node match {
    case PrintTime => emitValDef(sym,"System.err.println(\"[TIME]: \" + System.currentTimeMillis)")
    case _         => super.emitNode(sym,node)
  }
}
