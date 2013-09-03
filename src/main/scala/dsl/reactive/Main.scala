import dsl.reactive._
import scala.virtualization.lms.common._

trait ReactiveDSL extends Reactivity with ScalaOpsPkg with LiftScala
trait ReactiveDSLExp extends ReactiveDSL with ReactivityExpOpt with ScalaOpsPkgExp
trait ReactiveDSLGen extends ScalaGenReactivityOpt with ScalaCodeGenPkg {
  val IR: ReactiveDSLExp with ScalaOpsPkgExp
}

trait Program extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    val root = ReactiveVar(5)
    val roo2 = ReactiveVar(10)
    val s = ISignal { root.get + roo2.get }
    val constant = ISignal { 55 }

    println(s.get)
    constant
  }
}

object Main extends App {
  val prog = new Program with ReactiveDSLExp  with CompileScala { self =>
    override val codegen = new ReactiveDSLGen {
      val IR: self.type = self
    }
  }

  val f = prog.compile(prog.f)
  println(f())
}