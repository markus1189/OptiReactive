package dsl.reactive.examples

import dsl.reactive._
import dsl.reactive.auxiliary._
import dsl.reactive.optimizations._
import virtualization.lms.common._

import scala.collection.mutable.ListBuffer

trait SignalCodeGeneration extends ReactiveDSL {
  def exec(x: Rep[Unit]) = {
    val v = ReactiveVar(5)
    val s = ISignal { v.get + 1 }

    s
  }
}

object GenerateSignalCode extends App {
  val program = new SignalCodeGeneration
      with ReactiveDSLExpOpt
      with CompileScala { self =>

    override val codegen = new ReactiveDSLGenOpt {
      val IR: self.type = self
    }
  }

  program.codegen.emitSource(program.exec,
    "SignalCode",
    new java.io.PrintWriter(System.out))
}
