package dsl.reactive

import scala.virtualization.lms.common._

import dsl.reactive.syntaxops._
import dsl.reactive.optimizations._
import dsl.reactive.generalpurpose._

import dsl.reactive.phantom._

/** This trait defines the accepted syntactic constructs for programs
  * written in the DSL
  */
trait Reactivity
    extends MeasureOps
    with ExpensiveOps
    with VarSyntax
    with SignalSyntax
    with DepHolderSyntax
    with ReactiveEntitySyntax
    with InferredSignals
    with PointSyntax
    with RangeOps

/** This trait has to provide the concrete implementations for the
  * syntactically allowed constructs of the `Reactivity` trait
  */
trait ReactivityExp extends Reactivity
                    with MeasureOpsExp
                    with ExpensiveOpsExp
                    with ListOpsExp
                    with SeqOpsExp
                    with EffectExp
                    with InferredSignalsExp
                    with VarOps
                    with SignalOps
                    with DepHolderOps
                    with ReactiveEntityOps
                    with PointExp
                    with RangeOpsExp

trait ReactivityExpOpt extends ReactivityExp with ConstantFolding

trait ScalaGenReactivity extends ScalaGenReactiveBase
    with ScalaGenEffect
    with ScalaGenReactiveEntities
    with ScalaGenVars
    with ScalaGenSignals
    with ScalaGenDepHolders {

  val IR: ReactivityExp
  import IR._
}

trait ScalaGenReactivityOpt extends ScalaGenReactivity with ScalaGenConstantFolding {
  val IR: ReactivityExpOpt
  import IR._
}