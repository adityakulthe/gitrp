package org.planx.sh.parsing.hddl

import org.planx.sh.problem._
import org.planx.sh.solving._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical.StdTokenParsers

trait HDDLParser extends StdTokenParsers {
  type Tokens = HDDLTokens
  val lexical = new HDDLLexer

  // --- Section ADT for flexible parsing ---
  sealed trait Section
  case class Requirements(reqs: List[String]) extends Section
  case class Types(types: List[DomainType]) extends Section
  case class Predicates(preds: List[Predicate]) extends Section
  case class Functions(funcs: List[Function]) extends Section
  case class Tasks(tasks: List[DomainTask]) extends Section
  case class Action(act: DomainOperator) extends Section
  case class Method(meth: DomainMethod) extends Section
  case class Derived(ax: Axiom) extends Section

  // --- Entry point ---
  def parseDomain(input: String): ParseResult[Domain] =
    phrase(domain)(new lexical.Scanner(input))

  // --- Grammar ---

  lazy val domain: Parser[Domain] =
    "(" ~> "define" ~> domainBody <~ rep(")")

  lazy val domainBody: Parser[Domain] =
    rep(section) <~ ")" ^^ { sections =>
      val domName = sections.collectFirst { case Left(n) => n }.getOrElse("noname")
      val reqs    = sections.collectFirst { case Right(Requirements(r)) => r }.getOrElse(Nil)
      val types   = sections.collectFirst { case Right(Types(t)) => t }.getOrElse(Nil)
      val preds   = sections.collectFirst { case Right(Predicates(p)) => p }.getOrElse(Nil)
      val funcs   = sections.collectFirst { case Right(Functions(f)) => f }.getOrElse(Nil)
      val tasks   = sections.collectFirst { case Right(Tasks(t)) => t }.getOrElse(Nil)
      val ops     = sections.collect { case Right(Action(a)) => a }
      val meths   = sections.collect { case Right(Method(m)) => m }
      val axioms  = sections.collect { case Right(Derived(a)) => a }
      val pairedTasks = tasks.map(t => (t, meths.filter(_.name == t.name)))
      Domain(domName, reqs, types, preds, funcs, ops, pairedTasks, axioms)
    }

  lazy val section: Parser[Either[String, Section]] =
    ("(" ~> "domain" ~> name <~ ")") ^^ (Left(_)) |
    requirements ^^ (r => Right(Requirements(r))) |
    types ^^ (t => Right(Types(t))) |
    predicates ^^ (p => Right(Predicates(p))) |
    functions ^^ (f => Right(Functions(f))) |
    tasks ^^ (t => Right(Tasks(t))) |
    singleTask ^^ (t => Right(Tasks(List(t)))) | // <-- Add this line!
    derived ^^ (a => Right(Derived(a))) |  
    action ^^ (a => Right(Action(a))) |
    method ^^ (m => Right(Method(m)))

      

  // --- Requirements ---
  lazy val requirements: Parser[List[String]] =
    "(" ~> ":requirements" ~> rep(requireKey) <~ ")"
  lazy val requireKey: Parser[String] =
    (":strips" | ":typing" | ":negative-preconditions" | ":hierarchy" | ":method-preconditions" | ":equality" | ":universal-preconditions" | ":numeric-fluents" | ":derived-predicates")

  // --- Types ---
  lazy val types: Parser[List[DomainType]] =
    "(" ~> ":types" ~> rep1(name ~ opt("-" ~> name)) <~ ")" ^^ {
      _.map {
        case t ~ Some(supertype) => DomainType(t, supertype)
        case t ~ None           => DomainType(t, "object")
      }
    }

  // --- Variables with optional types ---
  lazy val variableWithOptionalType: Parser[(String, Option[String])] =
    (variable ~ opt("-" ~> name)) ^^ { case v ~ t => (v, t) }

  // --- Predicates ---
  lazy val predicates: Parser[List[Predicate]] =
    "(" ~> ":predicates" ~> rep(predicateSignature) <~ ")"
  lazy val predicateSignature: Parser[Predicate] =
    "(" ~> name ~ rep(variableWithOptionalType) <~ ")" ^^ {
      case n ~ vars =>
        Predicate(n, vars.map {
          case (v, Some(tpe)) => Var(Symbol(v), tpe)
          case (v, None)      => Var(Symbol(v), "object")
        })
    }

  // --- Functions (numeric fluents) ---
  lazy val functions: Parser[List[Function]] =
    "(" ~> ":functions" ~> rep(functionSignature) <~ ")" ^^ (_.toList)
  lazy val functionSignature: Parser[Function] =
    "(" ~> name ~ rep(variableWithOptionalType) <~ ")" ^^ {
      case n ~ vars =>
        Function(n, vars.map {
          case (v, Some(tpe)) => Var(Symbol(v), tpe)
          case (v, None)      => Var(Symbol(v), "object")
        })
    }
  // --- Single Task (non-standard, for compatibility) ---
  lazy val singleTask: Parser[DomainTask] =
    "(" ~> ":task" ~> name ~ opt(":parameters" ~> "(" ~> rep(variableWithOptionalType) <~ ")") <~ ")" ^^ {
      case n ~ varsOpt =>
        DomainTask(n, varsOpt.getOrElse(Nil).map {
          case (v, Some(tpe)) => Var(Symbol(v), tpe)
          case (v, None)      => Var(Symbol(v), "object")
        })
    }

  // --- Tasks ---
  lazy val tasks: Parser[List[DomainTask]] =
    "(" ~> ":tasks" ~> rep(taskSignature) <~ ")"
  lazy val taskSignature: Parser[DomainTask] =
    "(" ~> name ~ rep(variableWithOptionalType) <~ ")" ^^ {
      case n ~ vars =>
        DomainTask(n, vars.map {
          case (v, Some(tpe)) => Var(Symbol(v), tpe)
          case (v, None)      => Var(Symbol(v), "object")
        })
    }

  // --- Elements (actions, methods, axioms/derived) ---
  lazy val element: Parser[Any] = action | method | derived

  // --- Actions ---
  lazy val action: Parser[DomainOperator] =
    "(" ~> ":action" ~> name ~
      opt(parameters) ~
      opt(actionPrecondition) ~
      opt(actionEffect) <~ ")" ^^ {
    case n ~ paramsOpt ~ precondOpt ~ effectOpt =>
      val (addEffs, delEffs, assignEffs) = effectOpt.map(parseEffects).getOrElse((List(), List(), List()))
      DomainOperator(
        n,
        paramsOpt.getOrElse(Nil),
        precondOpt.getOrElse(ExpressionAtomic("true", new Bindable(Nil))),
        addEffs,
        delEffs,
        assignEffs,
        0.0
      )
  }

  // --- Parse effect expressions into add, delete, assignment ---
  private def parseEffects(expr: Expression): (List[Effect], List[Effect], List[Assignment]) = {
    def loop(e: Expression, add: List[Effect], del: List[Effect], assign: List[Assignment]): (List[Effect], List[Effect], List[Assignment]) = e match {
      case ExpressionAnd(l, r) =>
        val (addL, delL, assignL) = loop(l, add, del, assign)
        val (addR, delR, assignR) = loop(r, addL, delL, assignL)
        (addR, delR, assignR)
      case ExpressionAtomic(name, blueprint) if name.startsWith("not-") =>
        (add, Delete(Predicate(name.stripPrefix("not-"), blueprint.arguments)) :: del, assign)
      case ExpressionAtomic(name, blueprint) =>
        (Add(Predicate(name, blueprint.arguments)) :: add, del, assign)
      case ExpressionComparison(comparison) =>
        (add, del, NumericAssignment(comparison.comparator, comparison.arguments.head, comparison.arguments(1)) :: assign)
      case ConditionalEffect(cond, eff) =>
        (add, del, assign) // Extend if you want to collect conditional effects
      case _ => (add, del, assign)
    }
    loop(expr, List(), List(), List())
  }

  lazy val actionPrecondition: Parser[Expression] =
    ":precondition" ~> formula

  lazy val actionEffect: Parser[Expression] =
    ":effect" ~> formula

  // --- Methods ---
  lazy val method: Parser[DomainMethod] =
    "(" ~> ":method" ~> name ~
      opt(parameters) ~
      opt(methodTask) ~
      opt(methodPrecondition) ~
      subtasks <~ ")" ^^ {
    case n ~ _paramsOpt ~ _taskOpt ~ precondOpt ~ subtasks =>
      val precond = precondOpt.getOrElse(ExpressionAtomic("true", new Bindable(Nil)))
      DomainMethod(n, precond, subtasks)
  }

  // --- Parameters ---
  lazy val parameters: Parser[List[Var]] =
    ":parameters" ~> "(" ~> rep(variableWithOptionalType) <~ ")" ^^ {
      _.map {
        case (v, Some(tpe)) => Var(Symbol(v), tpe)
        case (v, None)      => Var(Symbol(v), "object")
      }
    }

  // --- Method task (optional) ---
  lazy val methodTask: Parser[Predicate] =
    ":task" ~> "(" ~> name ~ rep(term) <~ ")" ^^ { case n ~ ts => Predicate(n, ts) }

  // --- Method precondition ---
  lazy val methodPrecondition: Parser[Expression] =
    ":precondition" ~> formula

  // --- Subtasks (partial ordering supported) ---
  lazy val subtasks: Parser[TaskList] =
    (":ordered-subtasks" ~> subtasksList ^^ { ts => TaskList("sequence", ts) }
      | ":unordered-subtasks" ~> subtasksList ^^ { ts => TaskList("unordered", ts) }
      | ":subtasks" ~> subtasksList ^^ { ts => TaskList("unordered", ts) })

  lazy val subtasksList: Parser[List[Predicate]] =
    // Accept either (and (task1) (task2)) or just a list of tasks
    ("(" ~> "and" ~> rep1(taskAtom) <~ ")") | rep1(taskAtom)


  // --- Task atom ---
  lazy val taskAtom: Parser[Predicate] =
    "(" ~> name ~ rep(term) <~ ")" ^^ { case n ~ ts => Predicate(n, ts) }

  // --- Derived predicates / axioms ---
  lazy val derived: Parser[Axiom] =
    "(" ~> ":derived" ~> name ~ ("(" ~> rep(variableWithOptionalType) <~ ")") ~ formula <~ ")" ^^ {
      case n ~ vars ~ f =>
        new Axiom(n, Bindable(vars.map {
          case (v, Some(tpe)) => Var(Symbol(v), tpe)
          case (v, None)      => Var(Symbol(v), "object")
        })) { test(f) }
    }
  

  // --- Formula parsing (with universal quantification and conditional effects) ---
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

  // --- Numeric predicates ---
  lazy val numericPredicate: Parser[Expression] =
    "(" ~> numericOp ~ funExpression ~ funExpression <~ ")" ^^ {
      case op ~ t1 ~ t2 => ExpressionComparison(Comparison(op, List(t1, t2)))
    }
  lazy val numericOp: Parser[String] = "<=" | "<" | ">=" | ">" | "="

  // --- Terms ---
  lazy val terms: Parser[List[Term]] = rep1(term)
  lazy val term: Parser[Term] =
    (name ^^ Constant.apply
      | variable ^^ (v => Var(Symbol(v)))
      | number ^^ (n => Number(n.toDouble))
    )

  // --- FunExpression for equality and numeric expressions ---
  lazy val funExpression: Parser[FunExpression] =
    (name ^^ Constant.apply
      | variable ^^ (v => Var(Symbol(v)))
      | number ^^ (n => Number(n.toDouble))
    )

  // --- Tokens ---
  lazy val name: Parser[String] = accept("name", { case lexical.IdLit(s) => s.toLowerCase })
  lazy val variable: Parser[String] = accept("variable", { case lexical.VarIdLit(s) => s.toLowerCase.substring(1) })
  lazy val number: Parser[String] = accept("real", { case lexical.FloatLit(s) => s.toLowerCase }) | accept("int", { case lexical.IntLit(s) => s.toLowerCase })
}

// --- Helper class for conditional effects (extend as needed) ---
case class ConditionalEffect(condition: Expression, effect: Expression) extends Expression {
  def getBindings(state: State, binding: Binding) = effect.getBindings(state, binding)
}
