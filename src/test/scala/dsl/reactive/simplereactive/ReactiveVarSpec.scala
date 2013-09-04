package dsl.reactive.optimizations

import org.scalatest._
import dsl.reactive.simplereactive._

class ReactiveVarSpec extends WordSpec with Matchers {
  "ReactivVars" should {
    "return the initial value " in {
      val v = ReactiveVar(1)
      v.get should equal(1)
    }

    "return the new value after setting one" in {
      val v = ReactiveVar(1)
      v.set(42)
      v.get should equal(42)
    }

    "update the value when applying a function " in {
      val v = ReactiveVar(1)
      v.modify(_ + 1)
      v.get should equal(2)
    }

    "not notify dependents if the new value equals the old" in {
      var changes = 0
      val v = ReactiveVar(5)
      val stub = StubDependent(changes += 1)
      v.addDependent(stub)

      v.set(5)

      changes should equal(0)
    }

    "notify its dependents on actual changes" in {
      var changes = 0
      val v = ReactiveVar(5)
      val stub = StubDependent(changes += 1)
      v.addDependent(stub)

      v.set(6)

      changes should equal(1)
    }
  }
}