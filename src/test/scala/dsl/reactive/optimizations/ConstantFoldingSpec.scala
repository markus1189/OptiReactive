package dsl.reactive.optimizations

import org.scalatest._
import virtualization.lms.common.CompileScala
import dsl.reactive.{simplereactive => sr}

import dsl.reactive._

class ConstantFoldingSpec extends WordSpec with Matchers {
  "Constant Folding" should {
    "do nothing, if any of the dependents is not constant" in {
      val prog = new CanNotBeFolded with ReactiveDSLExp with CompileScala { self =>
        override val codegen = new ReactiveDSLGen {
          val IR: self.type = self
        }
      }

      val result = {
        prog.compile(prog.f).apply( () )
      }.asInstanceOf[sr.Behavior[Int]]

      result.isInstanceOf[sr.Signal[Int]] should equal(true)
    }

    "replace with a constant if all dependencies are constant" in {
      val prog = new ShouldBeFolded with ReactiveDSLExpOpt with CompileScala { self =>
        override val codegen = new ReactiveDSLGenOpt {
          val IR: self.type = self
        }
      }

      val result = {
        prog.compile(prog.f).apply( () )
      }.asInstanceOf[sr.Behavior[Int]]

      result.isInstanceOf[sr.Constant[Int]] should equal(true)
    }

    "replace with a constant, if the reactive only depends on other constants" in {
      val prog = new TransitiveFold with ReactiveDSLExpOpt with CompileScala { self =>
        override val codegen = new ReactiveDSLGenOpt {
          val IR: self.type = self
        }
      }

      val result = {
        prog.compile(prog.f).apply( () )
      }.asInstanceOf[sr.Behavior[Int]]

      result.isInstanceOf[sr.Constant[Int]] should equal(true)
    }
  }
}

trait CanNotBeFolded extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    val notConstant = ReactiveVar(5)
    ISignal { notConstant.get }
  }
}

trait ShouldBeFolded extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    ISignal { 42 }
  }
}

trait TransitiveFold extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    val s1 = ISignal { 42 }
    ISignal { s1.get + 1}
  }
}
