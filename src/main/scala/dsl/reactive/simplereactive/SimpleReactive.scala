package dsl.reactive.simplereactive

import language.implicitConversions
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer

trait ReactiveEntity {
  def getDependentsList: List[ReactiveEntity]
  def forceReEval(): Unit = ()
}

/* A node that has nodes that depend on it */
trait DepHolder extends ReactiveEntity {
  private val dependents: Buffer[Dependent] = ListBuffer()

  def addDependent(dep: Dependent) { dependents += dep }
  def <+ : Dependent => Unit = addDependent _

  def removeDependent(dep: Dependent) { dependents -= dep }

  def notifyDependents() { dependents.par.foreach(_.dependsOnChanged(this)) }

  def getDependentsList: List[ReactiveEntity] = dependents.toList
}

trait AccessableDepHolder[+A] extends DepHolder {
  def get: A
  def apply(): A = get
  def map[B](f: A => B): AccessableDepHolder[B]
}

/* A node that depends on other nodes */
trait Dependent extends ReactiveEntity {
  private val dependOn: Buffer[DepHolder] = new ListBuffer()

  def getDependendOnList: List[DepHolder] = dependOn.toList

  def addDependOn(dep: DepHolder) { dependOn += dep }
  def +> : DepHolder => Unit = addDependOn _

  def removeDependOn(dep: DepHolder) { dependOn -= dep }

  /* A node on which this one depends is changed */
  def dependsOnChanged(dep: DepHolder)
}

class ReactiveVar[A] private (initialValue: A) extends AccessableDepHolder[A] {
  private var heldValue: A = initialValue

  def get = heldValue

  def set(newValue: A) {
    if (newValue != heldValue) {
      heldValue = newValue
      notifyDependents()
    }
  }

  def modify(f: A => A) = set(f(get))

  def map[B](f: A => B): ReactiveVar[B] = ReactiveVar(f(initialValue))
}

object ReactiveVar {
  def apply[A](initialValue: A): ReactiveVar[A] = new ReactiveVar(initialValue)
}

trait Behavior[+A] extends Dependent with AccessableDepHolder[A]

class Signal[+A] private (depHolders: Seq[DepHolder])(expr: => A) extends Behavior[A] {

  private[this] var heldValue = expr

  def get: A = heldValue

  depHolders foreach addDependOn
  depHolders foreach (_.addDependent(this)) // check

  private def reEvaluate() {
    val evaluated = expr

    if (evaluated != heldValue) {
      heldValue = evaluated
      notifyDependents
    }
  }

  override def forceReEval() = reEvaluate()

  def dependsOnChanged(dep: DepHolder) { reEvaluate() }

  def map[B](f: A => B): Signal[B] = Signal(depHolders: _*)(f(expr))
}

object Signal {
  def apply[A](depHolders: DepHolder*)(expr: => A): Signal[A] =
    new Signal(depHolders)(expr)
}

class Constant[+A] private (expr: A) extends Behavior[A] {
  def get: A = expr
  def dependsOnChanged(dep: DepHolder): Unit = ()

  def map[B](f: A => B): Constant[B] = Constant(f(expr))
}

object Constant {
  def apply[A](expr: => A): Constant[A] = new Constant(expr)
}