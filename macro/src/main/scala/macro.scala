package MacroSandbox

import scala.reflect.macros.whitebox.Context //consider blackbox
import scala.language.experimental.macros

object PMacro {
  def myprintf(format: String, params: Any*): Unit = macro PMacro.myprintf_impl
  def myprintf_impl(c: Context)(format: c.Expr[String], params: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._
    val mystring = "test2"
    c.Expr[Unit](q"println($mystring)")
  }

  def debug[T](x: =>T): T = macro debug_impl[T]
  def debug_impl[T](c: Context)(x: c.Tree): c.Expr[T] = {
    import c.universe._

    val message = s"""
MACRO DEBUG CODE EXPANSION, AST DUMP, AND CALL SITE
-------------------------------------------------------------------------------------
EXPANDED CODE:
${showCode(x)}
-------------------------------------------------------------------------------------
AST: (with identifiers)
${showRaw(x, printIds=true)}
-------------------------------------------------------------------------------------
CALL SITE:
"""
    c.echo(c.enclosingPosition, message)
    println("")
    c.Expr[T](x)
  }

  def name[T](prefix: String)(x: =>T): T = macro name_impl[T]
  def name_impl[T: c.WeakTypeTag](c: Context)(prefix: c.Tree)(x: c.Tree) = {
    import c.universe._

    val prefix_empty = prefix match {
      case Literal(Constant(v)) if v == "" => true
      case _ => false
    } // For some reason: val es=""; q"$es" == prefix didn't work
      //   so just do the pattern matching manually
    val us: String = "_" // need this for later
    
    object vdoer extends Transformer {
      override def transform(tree: Tree) = tree match {
        case vdef @ q"$mods val $tname: $ttree = $assign" if mods == (Modifiers()) => {
          // Check Modifiers so Only match for val x = ...
          //   as ValDef also used in Class, Function, etc. definitions
          // vdef is no longer used but remains extracted to demonstrate the syntax for it
          val TermName(name) = tname
          val suggestion = (if(prefix_empty) q"$name" else q"$prefix+$us+$name")
          val subtree = transform(assign)
          q"$mods val $tname: $ttree = _root_.MacroSandbox.Name($subtree, $suggestion)"
        }
        case _ => {
          super.transform(tree)
        }
      }
    }

    import org.scalamacros.resetallattrs._ // VERY DANGEROUS: CONSIDER REMOVAL
    // erases many compiler symbols (forcing compiler to redo them)
    // thus breaking some code
    
    c.resetAllAttrs(vdoer.transform(x))
  }

  def classtest(fname: String): Test = macro classtest_impl
  def classtest_impl(c: Context)(fname: c.Tree) = {
    import c.universe._
    val classname = TypeName(c.freshName("SubTest"))
    val Literal(Constant(fieldname: String)) = fname // can't seem to get quasiquotes unapply to work here
    val fieldterm = TermName(fieldname)
    q"""
      class $classname(test: Int) extends Test {
        val $fieldterm = test
        def identity[T](in: T) = in
        def copy: this.type = (new $classname(test)).asInstanceOf[this.type]
      }
      new $classname(3)
    """
  }

  def imptest[T: c.WeakTypeTag](c: Context) = {
    import c.universe._
    val nameT = weakTypeOf[T]
    val tagT = implicitly[WeakTypeTag[T]]
    //c.echo(c.enclosingPosition, "Cannot construct Showable[$nameT] from $tagT")
    //c.error(c.enclosingPosition, "Cannot construct Showable[$nameT] from $tagT")
    // If error called then implicit construction aborted and implicitNotFound message printed
    //    this is likely preferable. In this case, error message not printed (but echo is)

    q"""
      new Showable[$nameT] {
        def show(in: $nameT) = in.toString
      }
    """
  }
  
  def gettypetest[T](x: =>T): String = macro gettype_impl[T]
  def gettype_impl[T: c.WeakTypeTag](c: Context)(x: c.Tree) = {
    import c.universe._

    //val checked = c.typecheck(x.duplicate) // haven't needed this yet
    // macro annotation may need this as it supposedly gets barer trees
    val q"..$unpack" = x
    unpack.foreach(_ match {
      case vdef @ q"$mods val $vname: $ttree = $assign" => {
        val istest = ttree.tpe <:< typeOf[MacroSandbox.Test]
        val tname = ttree.toString + (if(istest) " (is Test)" else "")
        println(s"$vname is $tname")
      }
      case _ => 
    })

    val thetype: String = "blah"
    q"$thetype"
  }
  
}

class Test {
  val i = 1
}

@annotation.implicitNotFound("Cannot find Showable implementation for ${A}")
trait Showable[A] {def show(in: A): String}
object Showable {
  implicit def materializeShowable[T]: Showable[T] = macro PMacro.imptest[T]
}
