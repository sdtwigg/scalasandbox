package MacroSandbox

import scala.reflect.macros.whitebox.Context //consider blackbox
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

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
  
  def gettypetest[T](x: =>T): Unit = macro gettype_impl[T]
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
    
    q"{}"
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

class probe extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro macroAnno.probeimpl
}
class node extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro macroAnno.nameimpl
}
class addclone extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro macroAnno.addcloneimpl
}

object macroAnno {
  def probeimpl(c: Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    import Flag._
    
    //println(s"#trees= ${annottees.length}")
    //annottees.foreach(println(_))

    // the typecheck and untypecheck makes sure inner annotations are expanded... may be risky???
    // so will just keep typed version aside as a reference???
    // but then potentially trickier to deal with expanded versus annotations/defmacros
    val shadow_typed = annottees.map(a=>c.typecheck(a.duplicate))
    
    object vdoer extends Transformer {
      override def transform(tree: Tree) = tree match {
        case q"$mods object $_ extends {..$_} with ..$_ {$_ => ..$_}" if mods.hasFlag(SYNTHETIC) => {
          println("Found Synthetic Object")
          q""
        }
        case _ => super.transform(tree)
      }
    }

    println(s"#trees= ${shadow_typed.length}")
    shadow_typed.foreach(println(_))
    
    val typed_xform = shadow_typed.map(vdoer.transform(_))
    //println(s"#trees= ${typed_xform.length}")
    //typed_xform.foreach(println(_))

    q"..$annottees"
  }
  
  def nameimpl(c: Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    
    object vdoer extends Transformer {
      override def transform(tree: Tree) = tree match {
        case q"$mods val $tname: $ttree = $assign" if mods == (Modifiers()) => {
          // Check Modifiers so Only match for val x = ...
          //   as ValDef also used in Class, Function, etc. definitions
          val TermName(name) = tname
          val suggestion = q"$name"
          val subtree = transform(assign)
          q"$mods val $tname: $ttree = _root_.MacroSandbox.Name($subtree, $suggestion)"
        }
        case _ => super.transform(tree)
      }
    }

    val named = annottees.map(vdoer.transform(_))
    q"..$named"
  }
  
  def addcloneimpl(c: Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    import Flag._

    def getTermName(in: ValDef): TermName = in match {case q"$mods val $tname: $tpt = $expr" => tname}

    val xformd: Seq[Tree] = annottees.map(_ match {
      case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" => {
        val termparamss = paramss.map(a=>a.map(b=>getTermName(b)))
        val myclone = q"override def clone: this.type = (new $tpname(...$termparamss)).asInstanceOf[this.type]"
        q"""
          $mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents {
            $self =>
              ..$stats
              $myclone
          }
        """
      }
      case other => other
    })

    q"..$xformd"
  }
}
