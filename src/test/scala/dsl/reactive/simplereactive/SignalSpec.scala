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

    "create a new signal when mapping" in {
      val s = Signal() { 5 }
      val s2 = s.map(_ + 1)

    }
  }

  "A Signal" when {
    "mapping" should {
      "create a new Signal" in {
        val s = Signal() { 1 }
        s.map(_ + 1) should not equal(s)
      }
    }

    "creating a new signal via map" should {
      "update if dependencies change" in {
        val v = ReactiveVar(5)
        val s1 = Signal(v) { v.get }
        val s2 = s1.map(_ + 1)

        v.set(10)

        s2.get should equal(11)
      }

      "create multiple signals" in {
        var created = 0
        val s = Signal() { created += 1; 1 } map (_ + 1) map (_ * 2) map (_ + 1)

        created should equal(4)
      }
    }

  }
}