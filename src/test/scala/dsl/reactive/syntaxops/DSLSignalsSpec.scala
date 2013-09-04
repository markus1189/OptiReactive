package dsl.reactive.optimizations

import org.scalatest._
import virtualization.lms.common.CompileScala
import dsl.reactive.{simplereactive => sr}

import dsl.reactive._

class DSLSignalsSpec extends WordSpec with Matchers {
  "An OptiReactive Signal" should {
    "support mapping" in {
      val prog = new MapSignal with ReactiveDSLExp with CompileScala { self =>
        override val codegen = new ReactiveDSLGen {
          val IR: self.type = self
        }

      }

      val result = {
        prog.compile(prog.f).apply( () )
      }.asInstanceOf[sr.Behavior[Int]]

      result.get should equal(42)
    }
  }
}

trait MapSignal extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    val v = ReactiveVar(41)
    val s = ISignal { v.get }

    s.map(x => x + 1)
  }
}