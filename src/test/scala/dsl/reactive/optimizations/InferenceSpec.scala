package dsl.reactive.optimizations

import org.scalatest._
import virtualization.lms.common.CompileScala
import dsl.reactive.{simplereactive => sr}

import dsl.reactive._

class InferenceSpec extends WordSpec with Matchers {
  "The dependency inference" should {
    "infer the correct dependencies" in {
      val prog = new InferenceTestCase with ReactiveDSLExp with CompileScala { self =>
        override val codegen = new ReactiveDSLGen {
          val IR: self.type = self
        }

      }

      val result = {
        prog.compile(prog.f).apply( () )
      }.asInstanceOf[sr.Behavior[Int]]

      result.getDependendOnList should have length(3)
    }

    "infer the evaluated branch in case of unstaged if condition" in {
      val prog = new DynamicInferenceUnstaged with ReactiveDSLExp
          with CompileScala { self =>

        override val codegen = new ReactiveDSLGen {
          val IR: self.type = self
        }

      }

      val result = {
        prog.compile(prog.f).apply( () )
      }.asInstanceOf[sr.Behavior[String]]

      result.getDependendOnList should have length(1)
      result.get should equal("true branch")
    }

    "infer both branches in case of a staged if condition" in {
      val prog = new DynamicInferenceStaged with ReactiveDSLExp
          with CompileScala { self =>

        override val codegen = new ReactiveDSLGen {
          val IR: self.type = self
        }

      }

      val result = {
        prog.compile(prog.f).apply( () )
      }.asInstanceOf[sr.Behavior[String]]

      result.getDependendOnList should have length(2)
      result.get should equal("true branch")
    }
  }
}

trait InferenceTestCase extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    val dep1 = ReactiveVar(1)
    val dep2 = ReactiveVar(2)
    val dep3 = ReactiveVar(3)

    ISignal { dep1.get + dep2.get + dep3.get }
  }
}

trait DynamicInferenceUnstaged extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    val condition = true

    val dep1 = ReactiveVar("true branch")
    val dep2 = ReactiveVar("false branch")

    ISignal { if(condition) dep1.get else dep2.get }
  }
}

trait DynamicInferenceStaged extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    val condition = unit(true)

    val dep1 = ReactiveVar("true branch")
    val dep2 = ReactiveVar("false branch")

    ISignal { if(condition) dep1.get else dep2.get }
  }
}
