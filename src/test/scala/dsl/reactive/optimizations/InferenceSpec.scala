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