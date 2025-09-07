package org.planx.sh.parsing.hddl

import scala.util.matching.Regex
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.token.StdTokens

trait HDDLTokens extends StdTokens {
  case class IdLit(chars: String) extends Token {
    override def toString = chars
  }
  case class VarIdLit(chars: String) extends Token {
    override def toString = chars
  }
  case class FloatLit(chars: String) extends Token {
    override def toString = chars
  }
  case class IntLit(chars: String) extends Token {
    override def toString = chars
  }
}

class HDDLLexer extends StdLexical with HDDLTokens {
  import scala.util.parsing.input.CharArrayReader.EofCh

  /** Regex helper */
  def regex(r: Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      (r findPrefixMatchOf source.subSequence(offset, source.length)) match {
        case Some(matched) =>
          Success(source.subSequence(offset, offset + matched.end).toString, in.drop(matched.end))
        case None =>
          Failure("String matching regex `" + r + "' expected, but `" + in.first + "' found", in.drop(0))
      }
    }
  }

  /** Reserved keywords - standalone logic and language terms */
  reserved ++= List(
    "define", "domain", "problem",
    "and", "or", "not", "imply", "when", "exists", "forall"
  )

  /** Delimiters - keywords and symbols (sorted by descending length) */
  delimiters ++= List(
    ":requirements", ":negative-preconditions", ":disjunctive-preconditions", ":equality",
    ":universal-preconditions", ":fluents", ":numeric-fluents", ":conditional-effects",
    ":derived-predicates", ":predicates", ":types", ":tasks", ":methods", ":method",
    ":task", ":parameters", ":precondition", ":effect", ":objects", ":init", ":goal",
    ":subtasks", ":ordered-subtasks", ":action", ":hierarchy", ":method-preconditions", ":typing",
    ":and", ":or", ":not", ":imply", ":when", ":exists", ":forall", ":ordered",":htn",
    ":define", ":domain", ":problem", ":strips",
    ":functions",                
    ":increase", ":decrease",    
    ":assign", ":scale-up", ":scale-down",
    ":temporal-constraints",     
    "+", "-", "*", "/", "<", "<=", ">", ">=", "=", "!=", "^",
    "(", ")", "-"
  ).sortBy(-_.length)
  

  /** Token parsing logic */
  override def token: Parser[Token] = {
    (
      // Reserved symbols and keywords (must be first!)
      delim

      // Variables: ?x, ?item1
      | regex("""\?[a-zA-Z0-9_\-!?]+""".r) ^^ { VarIdLit(_) }

      // Floats and integers: 3.14, -0.1e+2, 10, -5
      | regex("""(\+|-)?([0-9]+(\.[0-9]+)?([eE](\+|-)?[0-9]+)?)""".r) ^^ { s =>
        if (s.contains(".") || s.toLowerCase.contains("e")) FloatLit(s)
        else IntLit(s)
      }

      // Identifiers: move, pickup, robot1
      | regex("""[a-zA-Z_][a-zA-Z0-9_\-!?]*""".r) ^^ { process(_) }

      // End of file
      | EofCh ^^^ EOF
    )
  }

  /** Distinguish keyword vs identifier */
  def process(name: String) =
    if (reserved contains name) Keyword(name) else IdLit(name)

  /** Handle whitespace and comments */
  override def whitespace: Parser[Any] = rep(
    whitespaceChar
      | '/' ~ '*' ~ comment                      // Block comment
      | ';' ~ rep(chrExcept('\n', '\r', EofCh))  // Line comment
      | '/' ~ '*' ~> failure("Unclosed comment")
  )

  /** Block comment logic */
  override protected def comment: Parser[Any] = (
    '*' ~ '/' ^^ { _ => ' ' }
      | chrExcept(EofCh) ~ comment
  )
}
