import dsl.reactive._
import dsl.reactive.simplereactive._
import scala.virtualization.lms.common._

trait ReactiveDSL extends Reactivity with ScalaOpsPkg with LiftScala
trait ReactiveDSLExp extends ReactiveDSL with ReactivityExp with ScalaOpsPkgExp
trait ReactiveDSLGen extends ScalaGenReactivity with ScalaCodeGenPkg {
  val IR: ReactiveDSLExp with ScalaOpsPkgExp
}

trait Program extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    val root = ReactiveVar(5)
    val s = ISignal { root.get + 1 }

    println(s.get)
  }
}

object Main extends App {
  val prog = new Program with ReactiveDSLExp  with CompileScala { self =>
    override val codegen = new ReactiveDSLGen {
      val IR: self.type = self
    }
  }

  val f = prog.compile(prog.f)
  f()
}