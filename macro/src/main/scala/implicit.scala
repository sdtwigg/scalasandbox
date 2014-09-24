package ImplicitSandbox

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

trait Showable[A] {def show(in: A): String}
object Showable {
  implicit def materializeShowable[T]: Showable[T] = macro IMacro.imptest[T]
}

object IMacro {
  def imptest[T: c.WeakTypeTag](c: Context) = {
    import c.universe._
    val nameT = weakTypeOf[T]
    val nameString: String = nameT.toString
    val tagT = implicitly[WeakTypeTag[T]]
    //c.echo(c.enclosingPosition, "Cannot construct Showable[$nameT] from $tagT")
    //c.error(c.enclosingPosition, "Cannot construct Showable[$nameT] from $tagT")
    // If error called then implicit construction aborted and implicitNotFound message printed
    //    this is likely preferable. In this case, error message not printed (but echo is)

    q"""
      new Showable[$nameT] {
        def show(in: $nameT) = "ShowableMacro for " + $nameString
      }
    """
  }
}
