package dsl.reactive

import org.junit.Before
import org.junit.Test
import org.mockito.Mockito.{verify,times,never}
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar

import dsl.reactive.simplereactive._

class SignalTestSuite extends AssertionsForJUnit with MockitoSugar {

  var dh: simplereactive.DepHolder = _
  var s1: simplereactive.Signal[Int] = _
  var s2: simplereactive.Signal[Int] = _
  var s3: simplereactive.Signal[Int] = _

  @Before def initialize() {

    dh = new Object with simplereactive.DepHolder { def forceReEval {} }
    s1 = mock[simplereactive.Signal[Int]]
    s2 = mock[simplereactive.Signal[Int]]
    s3 = mock[simplereactive.Signal[Int]]

  }

  @Test def dependencyHolderNotifiesDependentsWhenNotifyDependentsIsCalled() {

    dh.addDependent(s1)
    dh.addDependent(s2)
    dh.addDependent(s3)
    dh.notifyDependents

    verify(s1).dependsOnChanged(dh)
    verify(s2).dependsOnChanged(dh)
    verify(s3).dependsOnChanged(dh)

  }

  @Test def reEvaluateEvaluatesTheExpressionAndNotifiesDependentsOnlyIfExpressionChanges() {

    var i = 1
    var s = Signal[Int](s1) { i }

    s.addDependent(s2)
    s.addDependent(s3)

    i = 2
    s.dependsOnChanged(s1)

    verify(s2).dependsOnChanged(s)
    verify(s3).dependsOnChanged(s)

    assert(s.get == 2)

    s.dependsOnChanged(s1)

    verify(s2, times(1)).dependsOnChanged(s)
    verify(s3, times(1)).dependsOnChanged(s)
    assert(s.get == 2)

  }

  @Test def signalReEvaluatesTheExpression() {
    var i = 1
    var s = Signal[Int](s1) { i }
    i = 2
    s.dependsOnChanged(s1)
    assert(s.get == 2)
  }

  @Test def theExpressionIsNoteEvaluatedEveryTimeGetValIsCalled() {
    var a = 10
    var s = Signal()( 1 + 1 + a )
    assert(s.get === 12)
    a = 11
    assert(s.get === 12)
  }

  @Test def simpleSignalReturnsCorrectExpressions() {
    var s = Signal()( 1 + 1 + 1 )
    assert(s.get === 3)
  }

  @Test def dependentsAreRemoved() {
    dh.addDependent(s1)
    dh.addDependent(s2)
    dh.removeDependent(s1)
    dh.notifyDependents

    verify(s1,never).dependsOnChanged(dh)
    verify(s2).dependsOnChanged(dh)
  }

}