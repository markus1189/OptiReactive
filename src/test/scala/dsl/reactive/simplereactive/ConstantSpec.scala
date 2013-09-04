package dsl.reactive.optimizations

import org.scalatest._
import dsl.reactive.simplereactive._

class ConstantSpec extends WordSpec with Matchers {
  "Constants" should {

    "return their stored value" in {
      val c = Constant(1)

      c.get should equal(1)
    }

    "not notify its dependents" in {
      var changes = 0
      val c = Constant(5)
      c.addDependent(StubDependent(changes += 1))  // changes = 1

      c.notifyDependents()

      changes should equal(1)
    }
  }
}