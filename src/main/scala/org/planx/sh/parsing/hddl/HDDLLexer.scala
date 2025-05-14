package org.planx.sh.parsing.hddl

import scala.util.matching.Regex
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.token.StdTokens

trait HDDLTokens extends StdTokens {
  case class IdLit(chars: String) extends Token { override def toString = chars }
  case class OpIdLit(chars: String) extends Token { override def toString = chars }
  case class VarIdLit(chars: String) extends Token { override def toString = chars }
  case class FloatLit(chars: String) extends Token { override def toString = chars }
  case class IntLit(chars: String) extends Token { override def toString = chars }
}

class HDDLLexer extends StdLexical with HDDLTokens {
  import scala.util.parsing.input.CharArrayReader.EofCh

  def regex(r: Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      (r findPrefixMatchOf (source.subSequence(offset, source.length))) match {
        case Some(matched) => Success(source.subSequence(offset, offset + matched.end).toString, in.drop(matched.end))
        case None => Failure("String matching regex `" + r + "' expected, but `" + in.first + "' found", in)
      }
    }
  }

  reserved ++= List(
    "and", "assign", "increase", "decrease", "define", "domain", "problem",
    "forall", "imply", "not", "or", "when", "task", "method", "subtasks", "ordering"
  )

  delimiters ++= List(
    "(", ")", "+", "-", "/", "*", ".", "=", "<", "<=", ">", ">=", "!=", "^",
    ":action", ":domain", ":task", ":tasks", ":method", ":parameters", ":precondition",
    ":effect", ":requirements", ":strips", ":typing", ":predicates", ":functions",
    ":types", ":constants", ":subtasks", ":ordering"
  )

  override def token: Parser[Token] = (
    regex("""[?][a-zA-Z0-9_\-]*""".r)                        ^^ { VarIdLit(_) }
    | regex("""[a-zA-Z_][a-zA-Z0-9_\-]*""".r)                ^^ { process(_) }
    | regex("""[+-]?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?""".r)^^ { FloatLit(_) }
    | regex("""[+-]?[0-9]+""".r)                             ^^ { IntLit(_) }
    | delim
    | regex("""[^\\s]+""".r)                                 ^^ { OpIdLit(_) }
    | EofCh ^^^ EOF
  )

  def process(name: String): Token = if (reserved contains name) Keyword(name) else IdLit(name)

  override def whitespace: Parser[Any] = rep(
    whitespaceChar
    | '/' ~ '*' ~ comment
    | ';' ~ rep(chrExcept('\n', '\r', EofCh))
    | '/' ~ '*' ~> failure("unclosed comment")
  )

  override protected def comment: Parser[Any] = (
    '*' ~ '/' ^^ { _ => ' ' }
    | chrExcept(EofCh) ~ comment
  )
}
