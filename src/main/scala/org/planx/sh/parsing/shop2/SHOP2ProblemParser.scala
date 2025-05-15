package org.planx.sh.parsing.shop2

import grizzled.slf4j.Logging
import org.planx.sh
import org.planx.sh.problem._
import org.planx.sh.solving.State
import org.planx.sh.utility.{DomainRequirements, Messaging}

import scala.collection.mutable.ListBuffer
import scala.io.Source._

class SHOP2ProblemParser extends SHOP2Parser with Logging {

  domain_parser_on = false
  dvars = new ListBuffer[Var]()

  // Entry point parser for full problem definition
  lazy val problem: Parser[Problem] = "(" ~> "define" ~> problem_name ~ domain_name ~ require_def ~ objects_dec ~ init ~ tasks <~ ")" ^^ {
    case pn ~ dn ~ reqs ~ objs ~ initState ~ goals =>
      for (o <- objs.objects) initState.add(o._type, Array(Constant(o._value)))
      sh.problem.Problem(pn, dn, reqs, objs, initState, goals)
  }

  lazy val problem_name = "(" ~> "problem" ~> name <~ ")"
  lazy val domain_name = "(" ~> ":domain" ~> name <~ ")"

  lazy val require_def = opt("(" ~> ":requirements" ~> rep(name) <~ ")") ^^ {
    case Some(reqs) => problem_requirements = reqs; reqs
    case None => List()
  }

  lazy val objects_dec = opt(objects_def_helper) ^^ {
    case Some(objs) => Objects(objs.flatten)
    case None => Objects()
  }
  lazy val supertype = "-" ~> name

  lazy val objects_def_helper = "(" ~> ":objects" ~> rep(objects_list) <~ ")"
  lazy val objects_list = rep1(name) ~ opt(supertype) ^^ {
    case names ~ Some(st) => names.map(n => ProblemObject(st, n))
    case names ~ None     => names.map(n => ProblemObject(DomainRequirements.OBJECT, n))
  }

  lazy val init: Parser[State] = "(" ~> ":init" ~> rep(init_elements) <~ ")" ^^ {
    case facts =>
      val state = new State
      facts.foreach { pred => state.add(pred.name, pred.arguments.toArray) }
      state
  }

  lazy val init_elements: Parser[Predicate] = simple_predicate | atomic_formula | init_fluent

  lazy val init_fluent: Parser[Predicate] = "(" ~> "=" ~> basic_function_term ~ fluent_value <~ ")" ^^ {
    case (fn, args) ~ value =>
      Predicate(fn, args :+ value)
  }

  lazy val basic_function_term: Parser[(String, List[Term])] =
    function_symbol ^^ { fn => (fn, List()) } |
    "(" ~> function_symbol ~ rep(name) <~ ")" ^^ {
      case fn ~ args => (fn, args.map(Constant))
    }

  lazy val fluent_value: Parser[Term] = number ^^ { n => Number(n.toDouble) } | name ^^ { Constant(_) }

  lazy val tasks: Parser[TaskList] = "(" ~> ":tasks" ~> task_list <~ ")"
}

object SHOP2ProblemParser extends SHOP2ProblemParser {

  private def parseStringToObject(input: String): Problem = {
    info(Messaging.printPlanningSystemMessage + "Processing the SHOP2 problem specification.")
    val scanner = new lexical.Scanner(input)
    phrase(problem)(scanner) match {
      case Success(result, _) =>
        info(Messaging.printPlanningSystemMessage + "The problem specification is correctly processed.")
        result
      case Failure(msg, next) =>
        error(Messaging.printPlanningSystemMessage + s"Failure at [${next.pos.line}.${next.pos.column}] - $msg")
        throw new RuntimeException("Parsing failed.")
      case Error(msg, next) =>
        error(Messaging.printPlanningSystemMessage + s"Error at [${next.pos.line}.${next.pos.column}] - $msg")
        throw new RuntimeException("Parsing failed.")
    }
  }

  private def parseFileToObject(file: String): Problem = parseStringToObject(fromFile(file).mkString)
  private def parseInputStreamToObject(is: java.io.InputStream): Problem = parseStringToObject(fromInputStream(is).mkString)

  def processProblemFileToObject(file: String): Problem = parseFileToObject(file)
  def processProblemStringToObject(input: String): Problem = parseStringToObject(input)
  def processProblemInputStreamToObject(is: java.io.InputStream): Problem = parseInputStreamToObject(is)
}
