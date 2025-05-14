package org.planx.sh.parsing.hddl

import grizzled.slf4j.Logging
import org.planx.sh.problem._
import org.planx.sh.solving._
import org.planx.sh.utility.{DomainRequirements, Messaging}

import java.io.InputStream
import scala.collection.mutable.{Set => MutableSet}
import scala.io.Source._

class HDDLDomainParser extends HDDLParser with Logging {

  domain_parser_on = true
  var dtypes: List[DomainType] = List()

  lazy val domain = "(" ~> "define" ~> domain_name ~ require_def ~ types_def ~ predicates_def ~ functions_def ~ structure_def <~ ")" ^^ {
    case dn ~ rd ~ td ~ pd ~ fd ~ structure =>
      val tasks: MutableSet[(DomainTask, List[DomainMethod])] = MutableSet()
      val operators: MutableSet[DomainOperator] = MutableSet()

      for (element <- structure) element match {
        case t: (DomainTask, List[DomainMethod]) => tasks += t
        case o: DomainOperator => operators += o
        case _ => // skip unknown
      }

      Domain(dn, rd, td, pd, fd, operators.toList.reverse, tasks.toList, List())
  }

  lazy val domain_name = "(" ~> "domain" ~> name <~ ")"

  lazy val types_def = opt("(" ~> ":types" ~> rep(typed_list) <~ ")") ^^ {
    case Some(types) =>
      val all = types.flatten
      dtypes = createDomainTypes(all, all)
      dtypes
    case None => List()
  }

  lazy val typed_list = rep1(name) ~ opt("-" ~> name) ^^ {
    case names ~ Some(parent) => names.map(n => DomainType(n, parent))
    case names ~ None         => names.map(n => DomainType(n, DomainRequirements.NOTYPE))
  }

  lazy val predicates_def = opt("(" ~> ":predicates" ~> rep1(atomic_formula_skeleton) <~ ")") ^^ {
    case Some(preds) => dpredicates = preds; preds
    case None => List()
  }

  lazy val atomic_formula_skeleton = "(" ~> predicate_name ~ predicate_args <~ ")" ^^ {
    case pn ~ args =>
      val flatArgs = args.flatten
      val argTypes = flatArgs.zipWithIndex.map { case (arg, idx) => (idx, arg._type) }.toSet
      Predicate(pn, flatArgs, scala.collection.mutable.Set(argTypes.toSeq: _*))
  }

  lazy val predicate_args = rep(typed_list_variable)

  lazy val typed_list_variable = rep1(variable) ~ opt("-" ~> name) ^^ {
    case vars ~ Some(st) => vars.map(v => { val temp = Var(Symbol(v), st); dvars += temp; temp })
    case vars ~ None     => vars.map(v => { val temp = Var(Symbol(v)); dvars += temp; temp })
  }

  lazy val functions_def = opt("(" ~> ":functions" ~> function_typed_list <~ ")") ^^ {
    case Some(funcs) => val all = funcs.flatten; dfunctions = all; all
    case None        => List()
  }

  lazy val function_typed_list = rep1("(" ~> name ~ rep(typed_list_variable) ~ (":" ~> "return-type" ~> name) <~ ")") ^^ {
    case lst => lst.map { case fn ~ args ~ retType =>
      List(Function(fn, args.flatten, retType))
    }
  }

  // === TASKS, METHODS, ACTIONS ===

  lazy val structure_def: Parser[List[Any]] = rep1(element)
  lazy val element = action_def | task_def | method_def

  lazy val task_def = "(" ~> ":task" ~> name ~ opt(parameters_def) <~ ")" ^^ {
    case name ~ Some(params) => (DomainTask(name, params.flatten), List())
    case name ~ None         => (DomainTask(name, List()), List())
  }

  lazy val method_def = "(" ~> ":method" ~> opt(name) ~ opt(parameters_def) ~ task_head ~ opt(pre_def) ~ subtasks ~ opt(ordering_spec) <~ ")" ^^ {
    case Some(methodName) ~ Some(params) ~ taskInfo ~ preOpt ~ subs ~ _ =>
      val (taskName, _) = taskInfo
      val task = DomainTask(taskName, params.flatten)
      val precond: Expression = preOpt match {
        case Some(p: Predicate) => ExpressionAtomic(p.name, Bindable(p.arguments))
        case Some(e: Expression) => e
        case _ => ExpressionNil()
      }
      
      val subtaskPredicates = subs.map { case (_, name, terms) => Predicate(name, terms) }
      val taskList = TaskList("sequence", subtaskPredicates)
      (task.name, List(DomainMethod(methodName, precond, taskList)))
  }

  lazy val parameters_def = ":parameters" ~> "(" ~> parameters <~ ")"
  lazy val parameters = rep(typed_list_variable)

  lazy val pre_def = ":precondition" ~> atomic_formula

  lazy val task_head = ":task" ~> "(" ~> name ~ rep(term) <~ ")" ^^ {
    case tn ~ args => (tn, args)
  }

  lazy val subtasks = ":subtasks" ~> "(" ~> rep(subtask) <~ ")"
  lazy val subtask = "(" ~> name ~ name ~ rep(term) <~ ")" ^^ {
    case id ~ name ~ args => (id, name, args)
  }

  lazy val ordering_spec = ":ordering" ~> rep("(" ~> name ~ name <~ ")")



  lazy val action_def = "(" ~> ":action" ~> name ~ parameters_def ~ pre_def ~ effect_def <~ ")" ^^ {
    case name ~ params ~ pre ~ eff =>
      val preExpr = pre match {
        case p: Predicate => ExpressionAtomic(p.name, Bindable(p.arguments))
        case e: Expression => e
        case _             => ExpressionNil()
      }
      DomainOperator(name, params.flatten, preExpr, eff, List(), List(), 0.0)
    
  }
  

  lazy val effect_def: Parser[List[Effect]] = ":effect" ~> rep(assign_effect)

  lazy val assign_effect: Parser[Effect] = "(" ~> assign_op ~ function_term ~ term <~ ")" ^^ {
    case op ~ func ~ value => NumericAssignment(op, func, value.asInstanceOf[FunExpression])
  }

  private def createDomainTypes(base: List[DomainType], derived: List[DomainType]): List[DomainType] = {
    if (derived.isEmpty) base
    else {
      val t = for {
        t1 <- base
        t2 <- derived
        if t1.supertype == t2.name
      } yield DomainType(t1.name, t2.supertype)
      createDomainTypes(base ::: t, t)
    }
  }
}

object HDDLDomainParser extends HDDLDomainParser {
  private def parseStringToObject(input: String) = {
    info(Messaging.printPlanningSystemMessage + "Processing the domain specification.")
    val scanner = new lexical.Scanner(input)
    phrase(domain)(scanner) match {
      case Success(result, _) => result
      case Failure(msg, next) =>
        error(Messaging.printPlanningSystemMessage + s"Failure at [${next.pos.line}.${next.pos.column}]: $msg")
        throw new RuntimeException("Domain parsing failed.")
      case Error(msg, next) =>
        error(Messaging.printPlanningSystemMessage + s"Error at [${next.pos.line}.${next.pos.column}]: $msg")
        throw new RuntimeException("Domain parsing failed.")
    }
  }

  private def parseFileToObject(file: String) = parseStringToObject(fromFile(file).mkString)

  def processDomainFileToObject(file: String): Domain = parseFileToObject(file) match {
    case d: Domain => d
    case _ => throw new RuntimeException("Processing failed.")
  }

  def processDomainStringToObject(input: String): Domain = parseStringToObject(input) match {
    case d: Domain => d
    case _ => throw new RuntimeException("Processing failed.")
  }

  def processDomainInputStreamToObject(is: InputStream): Domain = parseStringToObject(fromInputStream(is).mkString)
}
