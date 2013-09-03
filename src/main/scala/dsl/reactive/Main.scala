import dsl.reactive._
import dsl.reactive.simplereactive._
import scala.virtualization.lms.common._

trait Program extends Reactivity {
  def f(x : Rep[Unit]) = {
    val root = ReactiveVar(unit(5))
    val s = ISignal { root.get }

    println(s.get)

    s
  }
}

object Main extends App {
  val prog = new Program with ReactivityExp  with CompileScala { self =>
    override val codegen = new ScalaGenEffect with ScalaGenReactivity {
      val IR: self.type = self
    }
  }

  val f = prog.compile(prog.f)

  println(f)
}