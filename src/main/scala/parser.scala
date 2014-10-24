package ParseSandbox
import scala.util.parsing.combinator._


object parse_main {
  def run() = {
    println(ASTParser("Assign(x, 2)"))
    println(ASTParser("Block(Assign(x, 2), Assign(y, x) )"))
  }
}

sealed trait ASTexpression
case class ASTliteral(lit: Int) extends ASTexpression
case class ASTident(name: String) extends ASTexpression

sealed trait ASTstatement
case class ASTassign(ident: ASTident, rv: ASTexpression) extends ASTstatement
case class ASTblock(statements: Seq[ASTstatement]) extends ASTstatement

object ASTParser extends JavaTokenParsers {
  def astIdent   : Parser[ASTident]   = ident       ^^ { i => ASTident(i) }
  def astLiteral : Parser[ASTliteral] = wholeNumber ^^ { i => ASTliteral(i.toInt) }
  def astExpression : Parser[ASTexpression] = astLiteral | astIdent

  def astAssign       : Parser[ASTassign] = "Assign(" ~> astIdent ~ "," ~ astExpression <~ ")" ^^ {
    case ident ~ "," ~ expression => ASTassign(ident, expression)
  }
  def astBlock        : Parser[ASTblock]  = "Block(" ~> rep1sep(astStatement, ",")  <~ ")" ^^ {
    case statements => ASTblock(statements)
  }
  def astStatement : Parser[ASTstatement] = astAssign | astBlock

  def apply(input: String): ASTstatement = parseAll(astStatement, input) match {
    case Success(result, _)  => result
    case failure : NoSuccess => (scala.sys.error(failure.msg))
  }
}

// TODO: Add more elements, better expression tracking, and better error handling
