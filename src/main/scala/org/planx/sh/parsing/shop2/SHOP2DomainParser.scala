package org.planx.sh.parsing.shop2
import org.planx.sh.problem._
import org.planx.sh.solving._
import grizzled.slf4j.Logging
import org.planx.sh.problem._
import org.planx.sh.utility.Messaging
import scala.io.Source._
import java.io.InputStream
import scala.collection.mutable.{Set => MutableSet}

class SHOP2DomainParser extends SHOP2Parser with Logging {

  domain_parser_on = true
  var dtypes: List[DomainType] = List()

  // Entry point parser for the full domain
  lazy val domain: Parser[Domain] = "(" ~> "define" ~> domain_name ~ require_def ~ structure_def <~ ")" ^^ {
    case dn ~ reqs ~ structure =>
      val tasks: MutableSet[(DomainTask, List[DomainMethod])] = MutableSet()
      val operators: MutableSet[DomainOperator] = MutableSet()

      for (element <- structure) element match {
        case t: (DomainTask, List[DomainMethod]) => tasks += t
        case o: DomainOperator => operators += o
        case _ => // Ignore unrecognized
      }

      Domain(dn, reqs, dtypes, dpredicates, dfunctions, operators.toList.reverse, tasks.toList, List())
  }

  lazy val domain_name = "(" ~> "domain" ~> name <~ ")"

  // :requirements section
  lazy val require_def = opt("(" ~> ":requirements" ~> rep(name) <~ ")") ^^ {
    case Some(reqs) => domain_requirements = reqs; reqs
    case None => List()
  }

  // Structure = multiple operator and method definitions
  lazy val structure_def: Parser[List[Any]] = rep(operator_def | method_def)

  // SHOP2 :operator block → DomainOperator
  lazy val operator_def: Parser[DomainOperator] = "(" ~> ":operator" ~> name ~ parameters ~ opt(pre_def) ~ opt(effect_def) <~ ")" ^^ {
    case opName ~ params ~ preOpt ~ effOpt =>
      val precondition = preOpt.getOrElse(ExpressionNil())
      val effects = effOpt.getOrElse(List())
      DomainOperator(opName, params, precondition, effects, List(), List(), 0.0)
  }

  // SHOP2 :method block → DomainMethod (wrapped in DomainTask)
  lazy val method_def: Parser[(DomainTask, List[DomainMethod])] = "(" ~> ":method" ~> name ~ parameters ~ opt(pre_def) ~ subtasks <~ ")" ^^ {
    case methodName ~ params ~ preOpt ~ subs =>
      val task = DomainTask(methodName, params)
      val precondition = preOpt.getOrElse(ExpressionNil())
      val taskList = TaskList("sequence", subs) // SHOP2 defaults to sequence
      (task, List(DomainMethod(methodName, precondition, taskList)))
  }

  lazy val parameters = opt(":parameters" ~> "(" ~> rep(typed_var) <~ ")") ^^ {
    case Some(vars) => vars
    case None => List()
  }

  lazy val typed_var: Parser[Var] = variable ^^ { v =>
    val temp = Var(Symbol(v))
    dvars += temp
    temp
  }

  lazy val pre_def: Parser[Expression] = ":precondition" ~> pre_expr

  lazy val pre_expr: Parser[Expression] =
    "(" ~> "and" ~> rep1(simple_predicate) <~ ")" ^^ { preds =>
      val exprs: List[Expression] = preds.map(p => ExpressionAtomic(p.name, Bindable(p.arguments)))
      exprs match {
        case head :: Nil => head
        case head :: tail => tail.foldLeft(head) { (acc, e) => ExpressionAnd(acc, e) }
        case Nil => ExpressionNil()
      }
    } |
    atomic_formula ^^ { p => ExpressionAtomic(p.name, Bindable(p.arguments)) }
  
  
  
  

  lazy val effect_def: Parser[List[Effect]] = ":effect" ~> rep(assign_effect)

  lazy val assign_effect: Parser[Effect] = "(" ~> assign_op ~ function_term ~ term <~ ")" ^^ {
    case op ~ func ~ value => NumericAssignment(op, func, value.asInstanceOf[FunExpression])
  }

  lazy val subtasks: Parser[List[Predicate]] =
    ":subtasks" ~> "(" ~> rep(task_atom | simple_task_atom) <~ ")"
}

object SHOP2DomainParser extends SHOP2DomainParser {
  private def parseStringToObject(input: String): Domain = {
    info(Messaging.printPlanningSystemMessage + "Processing the SHOP2 domain specification.")
    val scanner = new lexical.Scanner(input)
    phrase(domain)(scanner) match {
      case Success(result, _) =>
        info(Messaging.printPlanningSystemMessage + "The domain specification is correctly processed.")
        result
      case Failure(msg, next) =>
        error(Messaging.printPlanningSystemMessage + s"Failure at [${next.pos.line}.${next.pos.column}]: $msg")
        throw new RuntimeException("Parsing failed.")
      case Error(msg, next) =>
        error(Messaging.printPlanningSystemMessage + s"Error at [${next.pos.line}.${next.pos.column}]: $msg")
        throw new RuntimeException("Parsing failed.")
    }
  }

  private def parseFileToObject(file: String): Domain = parseStringToObject(fromFile(file).mkString)
  private def parseInputStreamToObject(is: InputStream): Domain = parseStringToObject(fromInputStream(is).mkString)

  def processDomainFileToObject(file: String): Domain = parseFileToObject(file)
  def processDomainStringToObject(input: String): Domain = parseStringToObject(input)
  def processDomainInputStreamToObject(is: InputStream): Domain = parseInputStreamToObject(is)
}
