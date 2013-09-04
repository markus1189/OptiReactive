package dsl.reactive.optimizations

import org.scalatest._
import dsl.reactive.simplereactive._

class SignalSpec extends WordSpec with Matchers {
  "Signals" should {

    "return the correct result of the expression" in {
      val s = Signal() { 1 + 1 + 1 }

      s.get should equal(3)
    }

    "notify its dependents" in {
      var changes1 = 0
      var changes2 = 0
      val s = Signal() { 5 }
      val stub1 = StubDependent(changes1 += 1)
      val stub2 = StubDependent(changes2 += 1)
      s.addDependent(stub1)
      s.addDependent(stub2)

      s.notifyDependents()

      (changes1,changes2) should equal((1,1))
    }

    "evaluate the expression once on initialization" in {
      var evals = 0

      val s = Signal() { evals += 1}

      evals should equal(1)
    }

    "evaluate the expression after a change" in {
      var evals = 0
      val s = Signal() { evals += 1} // evals == 1

      s.dependsOnChanged(StubDepHolder.void)

      evals should equal(2)
    }

    "not notify dependents if the evaluated expression is the same as the old" in {
      var notifications = 0
      var i = 1
      val s = Signal() { i }
      s.addDependent(StubDependent(notifications += 1))

      s.forceReEval()

      notifications should equal(0)
    }

    "notify dependents if the expression evaluation yields a new value" in {
      var notifications = 0
      var i = 1
      val s = Signal() { i }
      s.addDependent(StubDependent(notifications += 1))
      i += 1

      s.forceReEval()

      notifications should equal(1)
    }

    "not evaluate the expression when get is called" in {
      var changes = 0
      var i = 0
      val s = Signal() { i }

      i = 42

      s.get should equal(0)  // still the old value, because no notifications occured
    }
  }
}