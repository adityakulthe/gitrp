package org.planx.sh.parsing.shop2

import scala.util.matching.Regex
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.input.CharArrayReader.EofCh

// ========== Tokens ==========
trait SHOP2Tokens extends StdTokens {
  case class IdLit(chars: String) extends Token { override def toString = chars }
  case class VarIdLit(chars: String) extends Token { override def toString = chars }
  case class FloatLit(chars: String) extends Token { override def toString = chars }
  case class IntLit(chars: String) extends Token { override def toString = chars }
}

// ========== Lexer ==========
class SHOP2Lexer extends StdLexical with SHOP2Tokens {

  // Reserved keywords used in SHOP2 domain and problem files
  reserved ++= List(
    "define", "domain", "problem",
    "and", "or", "not", "when", "forall", "assign",
    ":domain", ":requirements", ":operator", ":method",
    ":parameters", ":precondition", ":effect", ":subtasks",
    ":objects", ":init", ":tasks"
  )

  // Common delimiters and symbols
  delimiters ++= List(
    "(", ")", "+", "-", "*", "/", "=", "<", "<=", ">", ">=", "!=", "^"
  )

  // Token parser logic
  override def token: Parser[Token] = (
    regex("""[a-zA-Z_][a-zA-Z0-9_\-!?]*""".r) ^^ { process } |
    regex("""\?[a-zA-Z0-9_\-!?]*""".r)        ^^ { VarIdLit(_) } |
    regex("""(\+|-)?\d+\.\d+([eE](\+|-)?\d+)?""".r) ^^ { FloatLit(_) } |
    regex("""(\+|-)?\d+""".r)                ^^ { IntLit(_) } |
    delim |
    failure("illegal character")
  )

  // Token classification for identifiers
  def process(s: String): Token = {
    if (reserved contains s) Keyword(s) else IdLit(s)
  }

  // Whitespace and comments
  override def whitespace: Parser[Any] = rep(
    whitespaceChar |
    ';' ~ rep(chrExcept('\n', '\r', EofCh)) |
    '/' ~ '*' ~ comment |
    '/' ~ '*' ~> failure("unclosed comment")
  )

  override protected def comment: Parser[Any] = (
    '*' ~ '/' ^^ { _ => ' ' } |
    chrExcept(EofCh) ~ comment
  )

  // Regex utility for custom token matching
  def regex(r: Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      (r findPrefixMatchOf source.subSequence(offset, source.length)) match {
        case Some(matched) =>
          Success(source.subSequence(offset, offset + matched.end).toString, in.drop(matched.end))
        case None =>
          Failure(s"Expected pattern: $r", in)
      }
    }
  }
}
