package dsl.reactive.simplereactive

class StubDependent(f: () => Unit) extends Dependent {
  def dependsOnChanged(dep: DepHolder) = f()

  def getDependentsList = List()
}

object StubDependent {
  def apply(v: => Unit): StubDependent = new StubDependent(() => v)
  def void: StubDependent = new StubDependent(() => ())
}

class StubDepHolder(f: () => Unit) extends DepHolder {
  override def notifyDependents() = f()
}

object StubDepHolder {
  def apply(v: => Unit): StubDepHolder = new StubDepHolder(() => v)
  def void: StubDepHolder = new StubDepHolder(() => ())
}