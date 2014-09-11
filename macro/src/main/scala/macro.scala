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

    val message = s"""MACRO DEBUG CODE EXPANSION, AST DUMP, AND CALL SITE
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
  def name_impl[T: c.WeakTypeTag](c: Context)(prefix: c.Tree)(x: c.Tree): c.Expr[T] = {
    import c.universe._

    val prefix_empty = prefix match {
      case Literal(Constant(v)) if v == "" => true
      case _ => false
    } // For some reason: val es=""; q"$es" == prefix didn't work
      //   so just do the pattern matching manually
    val us: String = "_" // need this for later
    
    object vdoer extends Transformer {
      override def transform(tree: Tree) = tree match {
        case vdef @ ValDef(mods, tname, ttree, assign) if (mods == Modifiers())=> {
          // Check so Only match for val x = ...
          //   as ValDef also used in Class, Function, etc. definitions
          //val unmodded = (mods == Modifiers())
          val TermName(name) = tname
          val suggestion = (if(prefix_empty) q"$name" else q"$prefix+$us+$name")
          val subtree = transform(assign)
          val newassign = q"_root_.MacroSandbox.Name($subtree, $suggestion)"
          val newdef = ValDef(mods, tname, ttree, newassign)
          newdef
        }
        case _ => {
          super.transform(tree)
        }
      }
    }
/*    val q"..$stats" = x
    val cleaned = stats //stats.map(c.untypecheck(_))
    println(cleaned.map(showRaw(_, printIds=true)))
    println("")
*/
//    val transformed = vdoer.transformTrees(cleaned)
//    println(transformed.map(showRaw(_, printIds=true)))
//    println("")
    /*
    val loggedStats = stats.flatMap { stat =>
      val msg1 = "executing " + showCode(stat)
    //  val msg2 = "raw " + showRaw(stat)
    //  val msg3 = "tree " + stat.toString
    //  List(q"println($msg1)", q"println($msg2)", q"println($msg3)", stat)
      List(q"println($msg1)", stat)
    //  List(stat)
    }
    println(loggedStats.map(showRaw(_, printIds=true)))
    */

    import org.scalamacros.resetallattrs._ // VERY DANGEROUS: CONSIDER REMOVAL
    // erases many compiler symbols (forcing compiler to redo them)
    // thus breaking some code

//    val result = c.resetAllAttrs(q"..$transformed")
//    println(showRaw(result))
//    result
//    val result = vdoer.transform(x)
//    println(showCode(result))
    c.Expr[T](c.resetAllAttrs(vdoer.transform(x)))
  }
}

