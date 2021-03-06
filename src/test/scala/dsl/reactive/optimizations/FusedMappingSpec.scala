package dsl.reactive.optimizations

import org.scalatest._
import virtualization.lms.common.CompileScala
import dsl.reactive.{simplereactive => sr}

import dsl.reactive._

class FusedMappingSpec extends WordSpec with Matchers {
  "Fused Mapping" should {
    "not generate intermediate signals" in {
      val prog = new FusedMappingTestCase with ReactiveDSLExp with CompileScala with FusedMappingsOps { self =>
        override val codegen = new ReactiveDSLGen with ScalaGenFusedMapping {
          val IR: self.type = self
        }
      }

      val result = {
        prog.compile(prog.f).apply( () )
      }.asInstanceOf[sr.Behavior[Int]]

      val out = new java.io.StringWriter();
      prog.codegen.emitSource(prog.f, "F", new java.io.PrintWriter(out))
      val usedMaps = out.toString.lines.filter(_ contains("map")).toSeq
      val usedCompose = out.toString.lines.filter(_ contains("compose")).toSeq
      usedMaps should have length(1)
      usedCompose should have length(3)

      result.get should equal(43)
    }

    "be able to change the type of the signal" in {
      val prog = new FusedMappingChangeType with ReactiveDSLExp with CompileScala with FusedMappingsOps { self =>
        override val codegen = new ReactiveDSLGen with ScalaGenFusedMapping {
          val IR: self.type = self
        }
      }

      val result = {
        prog.compile(prog.f).apply( () )
      }.asInstanceOf[sr.Behavior[String]]

      result.get should equal("changed type to String")
    }

    "override the standard mapping function if trait is mixed in" in {
      val prog = new FusingRegularMaps
          with ReactiveDSLExp
          with CompileScala
          with FusedMappingsOps
          with FusedMappingsOverrides { self =>
        override val codegen = new ReactiveDSLGen with ScalaGenFusedMapping {
          val IR: self.type = self
        }
      }

      val out = new java.io.StringWriter();
      prog.codegen.emitSource(prog.f, "F", new java.io.PrintWriter(out))
      val splittedCode = out.toString.split("[ .]").toSeq

      splittedCode.filter(_ contains "map") should have length(1)
      splittedCode.filter(_ contains "compose") should have length(3)
    }
  }
}

trait FusedMappingTestCase extends ReactiveDSL with FusedMappings {
  def f(x : Rep[Unit]) = {
    val v = ReactiveVar(39)
    def inc(i: Rep[Int]): Rep[Int] = i + 1

    ISignal(v.get).fuseMap(inc).fuseMap(inc).fuseMap(inc).fuseMap(inc)
  }
}

trait FusedMappingChangeType extends ReactiveDSL with FusedMappings {
  def f(x : Rep[Unit]) = {
    val s = ISignal { 5 }
    s.fuseMap(_ => 42.42).
      fuseMap(_ => Seq(1,2,3)).
      fuseMap(_ => "changed type to String")
  }
}

trait FusingRegularMaps extends ReactiveDSL with FusedMappings  {
  def f(x : Rep[Unit]) = {
    val v = ReactiveVar(39)
    def inc(i: Rep[Int]): Rep[Int] = i + 1

    ISignal(v.get).map(inc).map(inc).map(inc).map(inc)
  }
}