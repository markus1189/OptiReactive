package dsl.reactive.examples

import dsl.reactive._
import dsl.reactive.auxiliary._
import dsl.reactive.optimizations._
import virtualization.lms.common._

trait ReactiveDSLSupport extends ReactiveDSLExp

trait MappingsCount0 extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    println("Number of maps: 0")
    printTime()

    val v = ReactiveVar(43l)
    val f: Rep[Long] => Rep[Long] = x => expensive(v.get)+x

    val s1 = ISignal(expensive(v.get))

    println(s1.get)
    printTime()

    v.set(42l)

    println(s1.get)
    printTime()
  }
}

trait MappingsCount1 extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    println("Number of maps: 1")
    printTime()

    val v = ReactiveVar(43l)
    val f: Rep[Long] => Rep[Long] = x => expensive(v.get)+x

    val s1 = ISignal(expensive(v.get))
    val s2 = s1.map(f)

    println(s2.get)
    printTime()

    v.set(42l)

    println(s2.get)
    printTime()
  }
}

trait MappingsCount2 extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    println("Number of maps: 2")
    printTime()

    val v = ReactiveVar(43l)
    val f: Rep[Long] => Rep[Long] = x => expensive(v.get)+x

    val s1 = ISignal(expensive(v.get))
    val s2 = s1.map(f)
    val s3 = s2.map(f)

    println(s3.get)
    printTime()

    v.set(42l)

    println(s3.get)
    printTime()
  }
}

trait MappingsCount3 extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    println("Number of maps: 3")
    printTime()

    val v = ReactiveVar(43l)
    val f: Rep[Long] => Rep[Long] = x => expensive(v.get)+x

    val s1 = ISignal(expensive(v.get))
    val s2 = s1.map(f)
    val s3 = s2.map(f)
    val s4 = s3.map(f)

    println(s4.get)
    printTime()

    v.set(42l)

    println(s4.get)
    printTime()
  }
}

trait MappingsCount4 extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    println("Number of maps: 4")
    printTime()

    val v = ReactiveVar(43l)
    val f: Rep[Long] => Rep[Long] = x => expensive(v.get)+x

    val s1 = ISignal(expensive(v.get))
    val s2 = s1.map(f)
    val s3 = s2.map(f)
    val s4 = s3.map(f)
    val s5 = s4.map(f)

    println(s5.get)
    printTime()

    v.set(42l)

    println(s5.get)
    printTime()
  }
}

trait MappingsCount5 extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    println("Number of maps: 5")
    printTime()

    val v = ReactiveVar(43l)
    val f: Rep[Long] => Rep[Long] = x => expensive(v.get)+x

    val s1 = ISignal(expensive(v.get))
    val s2 = s1.map(f)
    val s3 = s2.map(f)
    val s4 = s3.map(f)
    val s5 = s4.map(f)
    val s6 = s5.map(f)

    println(s6.get)
    printTime()

    v.set(42l)

    println(s6.get)
    printTime()
  }
}

object Main extends App {

  val p5 = new MappingsCount5
      with ReactiveDSLSupport
      with CompileScala { self =>

    override val codegen = new ReactiveDSLGen {
      val IR: self.type = self
    }
  }

  val p4 = new MappingsCount4
      with ReactiveDSLSupport
      with CompileScala { self =>

    override val codegen = new ReactiveDSLGen {
      val IR: self.type = self
    }
  }

  val p3 = new MappingsCount3
      with ReactiveDSLSupport
      with CompileScala { self =>

    override val codegen = new ReactiveDSLGen {
      val IR: self.type = self
    }
  }

  val p2 = new MappingsCount2
      with ReactiveDSLSupport
      with CompileScala { self =>

    override val codegen = new ReactiveDSLGen {
      val IR: self.type = self
    }
  }

  val p1 = new MappingsCount1
      with ReactiveDSLSupport
      with CompileScala { self =>

    override val codegen = new ReactiveDSLGen {
      val IR: self.type = self
    }
  }

  val p0 = new MappingsCount0
      with ReactiveDSLSupport
      with CompileScala { self =>

    override val codegen = new ReactiveDSLGen {
      val IR: self.type = self
    }
  }

  p0.compile(p0.f).apply()
  p1.compile(p1.f).apply()
  p2.compile(p2.f).apply()
  p3.compile(p3.f).apply()
  p4.compile(p4.f).apply()
  p5.compile(p5.f).apply()
}