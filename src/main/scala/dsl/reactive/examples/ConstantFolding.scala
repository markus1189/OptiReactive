package dsl.reactive.examples

import dsl.reactive._
import dsl.reactive.auxiliary._
import dsl.reactive.optimizations._
import virtualization.lms.common._

trait ConstantFoldingSimple extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    printTime()
    val v1 = ISignal { 43l }
    val v2 = ISignal { 43l }
    val v3 = ISignal { 43l }

    val s1 = ISignal { expensive(v1.get) + expensive(v2.get) + expensive(v3.get) }
    val s2 = ISignal { expensive(v1.get) + expensive(v2.get) + expensive(v3.get) }
    val s3 = ISignal { expensive(v1.get) + expensive(v2.get) + expensive(v3.get) }

    val r = ISignal { s1.get + s2.get + s3.get }

    println(r.get)
    printTime()
  }
}

object Main2 extends App {
  val optimized = new ConstantFoldingSimple
      with ReactiveDSLExpOpt
      with CompileScala { self =>

    override val codegen = new ReactiveDSLGenOpt {
      val IR: self.type = self
    }
  }

  val normal = new ConstantFoldingSimple
      with ReactiveDSLExp
      with CompileScala { self =>

    override val codegen = new ReactiveDSLGen {
      val IR: self.type = self
    }
  }

  optimized.compile(optimized.f).apply()
  optimized.codegen.emitSource(optimized.f, "F", new java.io.PrintWriter(System.out))
  normal.compile(normal.f).apply()
}