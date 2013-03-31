/*

 An S-Expression parser using Scala parser combinators.

 This S-Expression syntax supports integers and symbols.

 Author: Matthew Might
 Site:   http://matt.might.net/

 */

package languages.sexp ;

import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.syntax._
import scala.util.parsing.input.Positional  

// Abstract syntax tree nodes:
trait SExp extends Positional 

object SExpSyntax {
  case class L(var args : List[SExp]) extends SExp  // ( sx[1] ... sx[N] ).
  case class S(var symbol : String) extends SExp    // Symbol.
  case class Z(var z : BigInt) extends SExp          // Integer.
}

import SExpSyntax._


// Tokens for the lexer:
trait STokens extends Tokens {
  case class SSymbol(val chars : String) extends Token 
  case class SInteger(val chars : String) extends Token
  case class SPunct(val chars : String) extends Token 
}


// Lexer for S-Expressions:
class SLexical extends Lexical with STokens {
  import scala.util.parsing.input.CharArrayReader.EofCh
  
  def whitespace : Parser[Any] = rep(
      whitespaceChar
    | ';' ~ rep( chrExcept(EofCh, '\n') ) // Scheme-style comment.
    )

  def token : Parser[Token] = (
      ('(') ^^ { case c => SPunct(c.toString()) }
    | (')') ^^ { case c => SPunct(c.toString()) }
    | ('-' ~ rep1(digit)) ^^ { case '-' ~ chars => SInteger("-" + (chars mkString "")) }
    | (rep1(digit)) ^^ { case chars => SInteger(chars mkString "") }
    | (rep1(chrExcept(EofCh, '\r', '\n', '\t', ' ', '(', ')', ';'))) ^^ { case chars => SSymbol(chars mkString "") }
    | EofCh ^^^ EOF
    )
}


// The parser for S-Expressions:
object SParser extends TokenParsers {
  type Tokens = STokens
  val lexical = new SLexical
  
  import lexical.{SSymbol,SPunct,SInteger}

  // keyword: Automatically turn a string into an appropriate parser.
  implicit def keyword(chars: String): Parser[String] = chars match {
    case ("("|")") => accept(SPunct(chars)) ^^ (_.chars)
    case _ => accept(SSymbol(chars)) ^^ (_.chars)
  }
  
  /* Grammar. */

  // Start symbol:
  def prog : Parser[List[SExp]] = phrase(exp *) ^^ { case exps => exps }

  
  // Terminals: 
  def symbol : Parser[String] =
    elem("symbol", _.isInstanceOf[SSymbol]) ^^ (_.chars)

  def integer : Parser[BigInt] =
    elem("integer", _.isInstanceOf[SInteger]) ^^ { case s => BigInt(s.chars) }


  // Non-terminals: 
  def s : Parser[S] = positioned(symbol ^^ { case s => S(s) })

  def z : Parser[Z] = positioned(integer ^^ { case bi => Z(bi) }) 

  def list : Parser[L] = positioned(("(" ~ (exp *) ~ ")") ^^
    { case "(" ~ sxs ~ ")" => L(sxs) })

  def exp : Parser[SExp] = positioned((list | z | s) ^^ { case e => e })


  // External interface:
  def parse (s : String) : List[SExp] = {
    prog(new lexical.Scanner(s)) match {
      case Success(sx, _) => sx

      case Failure(msg,_) =>
       throw new Exception("parse failed: " + msg)

      case Error(msg,_) =>
       throw new Exception("parse error: " + msg)
    }
  }
}
