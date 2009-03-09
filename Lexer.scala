package scala_thrift

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.input.CharArrayReader.EofCh

class Lexer extends StdLexical with ImplicitConversions {
  override def letter = elem("letter", c => ((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')))
  def identChar = letter | elem('_')
  def stringLit1: Parser[StringLit] = '"' ~ rep(chrExcept('"', '\n', EofCh)) ~ '"' ^^ {
    case '"' ~ s ~ '"' => StringLit(s.mkString(""))
  }
  def stringLit2: Parser[StringLit] = '\'' ~ rep(chrExcept('\'', '\n', EofCh)) ~ '\'' ^^ {
    case '\'' ~ s ~ '\'' => StringLit(s.mkString(""))
  }
  def intLit = sign ~ rep1(digit) ^^ { case s ~ d => s + d.mkString("") }
  def doubleLit = sign ~ rep(digit) ~ opt(decPart) ~ opt(expPart) ^^ {
    case s ~ i ~ d ~ e => s + i.mkString("") + d + e
  }
  def sign = opt(elem("sign character", c => c == '-' || c == '+')) ^^ { _.map(_.toString).getOrElse("") }
  def exponent = elem("exponent character", c => c == 'e' || c == 'E')
  def decPart: Parser[String] = '.' ~ rep1(digit) ^^ {
    case '.' ~ d => "." + d.mkString("")
  }
  def expPart: Parser[String] = exponent ~ intLit ^^ {
    case e ~ i => e + i
  }

  override def token: Parser[Token] = (
      identChar ~ rep(identChar | '.' | '-') ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
    | intLit                                 ^^ IntLiteral
    | doubleLit                              ^^ DoubleLiteral
    | stringLit1
    | stringLit2
    | EofCh                                  ^^^ EOF
    | '\'' ~> failure("unclosed string literal")
    | '\"' ~> failure("unclosed string literal")
    | delim
    | failure("illegal character")
  )

  // def Identifier      = (letter | '_') ~ rep(letter | digit | '.' | '_')
  // def STIdentifier    = (letter | '_') ~ rep(letter | digit | '.' | '_' | '-')

  override def whitespace = rep(
      whitespaceChar
    | '/' ~ '*' ~ comment
    | '/' ~ '/' ~ rep(chrExcept(EofCh, '\n'))
    | '#' ~ rep(chrExcept(EofCh, '\n'))
    | '/' ~ '*' ~ failure("unclosed comment")
  )
  
  case class IntLiteral(override val chars: String) extends NumericLit(chars)
  case class DoubleLiteral(override val chars: String) extends NumericLit(chars)
}
