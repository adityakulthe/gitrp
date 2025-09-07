package org.planx.sh.parsing.hddl

import org.planx.sh.problem._
import org.planx.sh.solving._
import scala.util.parsing.combinator.syntactical.StdTokenParsers

trait HDDLProblemParser extends StdTokenParsers {
  type Tokens = HDDLTokens
  val lexical = new HDDLLexer

  // Entry point
  def parseProblem(input: String): ParseResult[Problem] =
    phrase(problem)(new lexical.Scanner(input))

  // Problem grammar
  lazy val problem: Parser[Problem] =
    "(" ~> "define" ~> problemBody <~ rep(")")

  lazy val problemBody: Parser[Problem] =
    ("(" ~> "problem" ~> name <~ ")") ~
      ("(" ~> ":domain" ~> name <~ ")") ~
      opt(objects) ~
      init ~
      opt(goal) ~
      opt(htn) <~ ")" ^^ {
        case probName ~ domainName ~ objectsOpt ~ initLits ~ goalOpt ~ htnOpt =>
          Problem(
            name = probName,
            domain = domainName,
            objects = objectsOpt.getOrElse(Nil),
            init = initLits,
            goal = goalOpt.getOrElse(ExpressionAtomic("true", new Bindable(Nil))),
            htn = htnOpt
          )
      }





  // Objects (typed or untyped)
  lazy val objects: Parser[List[TypedObject]] =
    "(" ~> ":objects" ~> rep1(typedObject) <~ ")"

  lazy val typedObject: Parser[TypedObject] =
    name ~ opt("-" ~> name) ^^ {
      case n ~ Some(tpe) => TypedObject(n, tpe)
      case n ~ None      => TypedObject(n, "object")
    }

  // Init
  lazy val init: Parser[List[InitLiteral]] =
    "(" ~> ":init" ~> rep(initLiteral) <~ ")"

  lazy val initLiteral: Parser[InitLiteral] =
    // Numeric assignment or predicate
    ("(" ~> "=" ~> funExpression ~ number <~ ")" ^^ { case f ~ n => InitAssign(f, n.toDouble) }) |
    ("(" ~> name ~ rep(term) <~ ")" ^^ { case n ~ ts => InitPredicate(n, ts) })

  // Goal
  lazy val goal: Parser[Expression] =
    "(" ~> ":goal" ~> formula <~ ")"

  // HTN/Tasks (optional, for HDDL)
  lazy val htn: Parser[HTN] =
    (("(" ~> (":htn" | ":tasks") ~> taskNetwork <~ ")") | (":htn" ~> taskNetwork))

  lazy val taskNetwork: Parser[HTN] =
    (":subtasks" ~> "(" ~> name ~ rep(term) <~ ")" ^^ { case n ~ ts => HTN(n, ts) }
    | "(" ~> name ~ rep(term) <~ ")" ^^ { case n ~ ts => HTN(n, ts) })


  // Formula (reuse from domain parser)
  lazy val formula: Parser[Expression] =
    ("(" ~> "and" ~> rep1(formula) <~ ")" ^^ { exprs => exprs.reduceLeft(ExpressionAnd) }
      | "(" ~> "or" ~> rep1(formula) <~ ")" ^^ { exprs => exprs.reduceLeft(ExpressionOr) }
      | "(" ~> "not" ~> formula <~ ")" ^^ { expr => ExpressionNot(expr) }
      | "(" ~> "imply" ~> formula ~ formula <~ ")" ^^ { case a ~ b => ExpressionImply(a, b) }
      | "(" ~> "forall" ~> (("(" ~> rep(variableWithOptionalType) <~ ")") ~ formula) <~ ")" ^^ {
          case vars ~ f =>
            ExpressionForall(
              vars.map {
                case (v, Some(tpe)) => Var(Symbol(v), tpe)
                case (v, None)      => Var(Symbol(v), "object")
              },
              ExpressionNil(),
              f
            )
        }
      | "(" ~> "when" ~> formula ~ formula <~ ")" ^^ { case cond ~ eff => ConditionalEffect(cond, eff) }
      | atomicFormula
    )

  lazy val atomicFormula: Parser[Expression] =
    simplePredicate | properPredicate | equalityPredicate | numericPredicate

  lazy val simplePredicate: Parser[Expression] =
    "(" ~> name <~ ")" ^^ { pn => ExpressionAtomic(pn, new Bindable(Nil)) }

  lazy val properPredicate: Parser[Expression] =
    "(" ~> name ~ rep(term) <~ ")" ^^ { case pn ~ ts => ExpressionAtomic(pn, new Bindable(ts)) }

  lazy val equalityPredicate: Parser[Expression] =
    "(" ~> "=" ~> funExpression ~ funExpression <~ ")" ^^ { case t1 ~ t2 =>
      ExpressionComparison(Comparison("=", List(t1, t2)))
    }

  // Numeric predicates
  lazy val numericPredicate: Parser[Expression] =
    "(" ~> numericOp ~ funExpression ~ funExpression <~ ")" ^^ {
      case op ~ t1 ~ t2 => ExpressionComparison(Comparison(op, List(t1, t2)))
    }
  lazy val numericOp: Parser[String] = "<=" | "<" | ">=" | ">" | "="

  // Terms
  lazy val terms: Parser[List[Term]] = rep1(term)
  lazy val term: Parser[Term] =
    (name ^^ Constant.apply
      | variable ^^ (v => Var(Symbol(v)))
      | number ^^ (n => Number(n.toDouble))
    )

  // FunExpression for equality and numeric expressions
  lazy val funExpression: Parser[FunExpression] =
    (name ^^ Constant.apply
      | variable ^^ (v => Var(Symbol(v)))
      | number ^^ (n => Number(n.toDouble))
    )

  // Variables with optional types (reuse from domain parser)
  lazy val variableWithOptionalType: Parser[(String, Option[String])] =
    (variable ~ opt("-" ~> name)) ^^ { case v ~ t => (v, t) }

  // Tokens
  lazy val name: Parser[String] = accept("name", { case lexical.IdLit(s) => s.toLowerCase })
  lazy val variable: Parser[String] = accept("variable", { case lexical.VarIdLit(s) => s.toLowerCase.substring(1) })
  lazy val number: Parser[String] = accept("real", { case lexical.FloatLit(s) => s.toLowerCase }) | accept("int", { case lexical.IntLit(s) => s.toLowerCase })
}

// --- Data classes for Problem ---

case class Problem(
  name: String,
  domain: String,
  objects: List[TypedObject],
  init: List[InitLiteral],
  goal: Expression,
  htn: Option[HTN]
)

case class TypedObject(name: String, tpe: String)
sealed trait InitLiteral
case class InitPredicate(name: String, terms: List[Term]) extends InitLiteral
case class InitAssign(f: FunExpression, value: Double) extends InitLiteral
case class HTN(name: String, args: List[Term])
