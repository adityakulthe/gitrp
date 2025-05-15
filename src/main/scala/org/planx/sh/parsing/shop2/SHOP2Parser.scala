package org.planx.sh.parsing.shop2

import org.planx.sh.problem._
import org.planx.sh.solving._

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StdTokenParsers

trait SHOP2Parser extends StdTokenParsers with ImplicitConversions with ExpressionConversions {
  type Tokens = SHOP2Tokens
  val lexical = new SHOP2Lexer

  protected var domain_parser_on = false
  protected var domain_requirements = List[String]()
  protected var problem_requirements = List[String]()

  var dpredicates: List[Predicate] = List()
  var dfunctions: List[Function] = List()
  var dvars = new ListBuffer[Var]()

  // ============================
  // ========== LEXING ==========
  // ============================

  lazy val name = accept("name", { case lexical.IdLit(s) => s.toLowerCase })
  lazy val variable = accept("string", { case lexical.VarIdLit(s) => s.toLowerCase.substring(1) })
  lazy val number = accept("real", { case lexical.FloatLit(s) => s.toLowerCase }) |
                    accept("int", { case lexical.IntLit(s) => s.toLowerCase })

  // ================================
  // ========== BASIC UNITS =========
  // ================================

  lazy val term: Parser[Term] =
    name ^^ { Constant(_) } |
    variable ^^ { v =>
      dvars.find(_.name == Symbol(v)).getOrElse(Var(Symbol(v)))
    } |
    number ^^ { n => Number(n.toDouble) } |
    f_exp_f_head
  

  lazy val terms: Parser[List[Term]] = rep1(term)

  lazy val predicate_name = name

  lazy val atomic_formula: Parser[Predicate] =
    "(" ~> predicate_name ~ terms <~ ")" ^^ { case pn ~ t => Predicate(pn, t) }

  lazy val simple_predicate: Parser[Predicate] =
    "(" ~> predicate_name <~ ")" ^^ { pn => Predicate(pn) }

  lazy val task_atom: Parser[Predicate] =
    "(" ~> name ~ terms <~ ")" ^^ { case n ~ t => Predicate(n, t) }

  lazy val simple_task_atom: Parser[Predicate] =
    "(" ~> name <~ ")" ^^ { n => Predicate(n) }

  lazy val task_list: Parser[TaskList] =
    "(" ~> ordering ~ rep(tl) <~ ")" ^^ { case o ~ ta => TaskList(o, ta) } |
    "(" ~> ")" ^^ { _ => TaskList("sequence", List()) }

  lazy val ordering = "sequence" | "unordered"
  lazy val assign_op = "assign" | "scale-up" | "scale-down" | "increase" | "decrease"

  lazy val tl: Parser[Predicate] = task_atom | simple_task_atom 

  // ================================
  // ========== FUNCTION ============
  // ================================

  lazy val function_symbol = name

  lazy val function_term = "(" ~> function_symbol ~ terms <~ ")" ^^ {
    case fs ~ t => Function(fs, t)
  }

  lazy val f_exp_f_head = "(" ~> function_symbol ~ opt(terms) <~ ")" ^^ {
    case fs ~ Some(t) =>
      val f = dfunctions.find(df => df.name == fs && df.args.size == t.size).getOrElse(
        throw new RuntimeException(s"Function $fs with ${t.size} args not declared")
      )
      val typeExpr = f.args.zip(t).collect {
        case (a: Var, tm) => a._type -> tm
      }
      Function(fs, t, f.returnType, typeExpr)

    case fs ~ None =>
      val f = dfunctions.find(df => df.name == fs && df.args.isEmpty).getOrElse(
        throw new RuntimeException(s"Function $fs with 0 args not declared")
      )
      Function(fs, Nil, f.returnType)
  }
}
