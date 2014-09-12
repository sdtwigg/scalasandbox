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
      class $classname extends Test {
        val $fieldterm = 3
        def copy: this.type = (new $classname).asInstanceOf[this.type]
      }
      new $classname
    """
  }
}

class Test {
  val i = 1
}
