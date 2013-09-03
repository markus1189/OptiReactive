package dsl.reactive

import org.junit.Before
import org.junit.Test
import org.mockito.Mockito.verify
import org.mockito.Mockito.times
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import dsl.reactive.simplereactive._

class VarTestSuite extends AssertionsForJUnit with MockitoSugar {

  var v1: simplereactive.ReactiveVar[Int] = _
  var v2: simplereactive.ReactiveVar[Int] = _
  var s1: simplereactive.Signal[Int] = _
  var s2: simplereactive.Signal[Int] = _
  var s3: simplereactive.Signal[Int] = _

  @Before def initialize() {

    v1 = ReactiveVar(1)
    v2 = ReactiveVar(2)
    s1 = mock[simplereactive.Signal[Int]]
    s2 = mock[simplereactive.Signal[Int]]
    s3 = mock[simplereactive.Signal[Int]]
  }

  @Test def getValAfterCreationReturnsInitializationValue() {
    val v = ReactiveVar(1)
    assert(v.get == 1)
  }

  @Test def getValReturnsCorrectValue() {
    val v = ReactiveVar(1)
    v.set(10)
    assert(v.get == 10)
  }

  @Test def varNotifiesSignalOfChanges() {
    val v = ReactiveVar(1)
    val s = Signal(v){ v.get + 1 }
    assert(v.get == 1)

    assert(s.get == 2)
    v.set(2)
    assert(v.get == 2)
    assert(s.get == 3)

  }

}