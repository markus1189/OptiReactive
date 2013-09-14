package dsl.reactive

import scala.virtualization.lms.common._

import dsl.reactive.syntaxops._
import dsl.reactive.optimizations._
import dsl.reactive.auxiliary._

import dsl.reactive.phantom._

/** ReactiveDSL is the main trait that should be used by clients to write programs */
trait ReactiveDSL extends Reactivity with ScalaOpsPkg with LiftScala

/** ReactiveDSLExp and ReactiveDSLGen are required for the concrete implementations
  * and the code generation
  */
trait ReactiveDSLExp extends ReactiveDSL with ReactivityExpOpt with ScalaOpsPkgExp
trait ReactiveDSLGen extends ScalaGenReactivityOpt with ScalaCodeGenPkg {
  val IR: ReactiveDSLExp with ScalaOpsPkgExp
}

/** This trait defines the accepted syntactic constructs for programs
  * written in the DSL, without lifting standard scala constructs
  */
trait Reactivity
    extends MeasureOps
    with ExpensiveOps
    with VarSyntax
    with SignalSyntax
    with DepHolderSyntax
    with ReactiveEntitySyntax
    with InferredSignals
    with FusedMappings
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
                    with FusedMappingsOps
                    with PointExp
                    with RangeOpsExp

/** */
trait ScalaGenReactivity extends ScalaGenReactiveBase
    with ScalaGenEffect
    with ScalaGenReactiveEntities
    with ScalaGenVars
    with ScalaGenSignals
    with ScalaGenDepHolders
    with ScalaGenFusedMapping {

  val IR: ReactivityExp
  import IR._
}

/** Like ReactivityExp but with ConstantFolding enabled */
trait ReactivityExpOpt extends ReactivityExp with ConstantFolding

/** Additional support for ConstantFolding code generation */
trait ScalaGenReactivityOpt extends ScalaGenReactivity with ScalaGenConstantFolding {
  val IR: ReactivityExpOpt
  import IR._
}