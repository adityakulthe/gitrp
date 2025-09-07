package org.planx.sh.parsing.hddl

import org.planx.sh.problem._
import org.planx.sh.utility.Messaging
import java.io.InputStream
import scala.io.Source._
import grizzled.slf4j.Logging
import scala.collection.mutable
import org.planx.sh.solving._

object HDDLDomainParser extends HDDLParser with Logging {

  // --- Pluggable Extension Hooks ---
  private var customHooks: List[Domain => Unit] = List()
  def registerCustomHook(hook: Domain => Unit): Unit = {
    customHooks = hook :: customHooks
  }
  private def runCustomHooks(domain: Domain): Unit = {
    customHooks.foreach(hook => hook(domain))
  }

  // --- Input Validation ---
  private def validateInput(input: String): Unit = {
    if (input.trim.isEmpty)
      throw new RuntimeException(Messaging.printPlanningSystemMessage + "Domain input is empty.")
    val openParens = input.count(_ == '(')
    val closeParens = input.count(_ == ')')
    if (openParens != closeParens)
      throw new RuntimeException(Messaging.printPlanningSystemMessage + s"Unbalanced parentheses: $openParens '(' and $closeParens ')'.")
    if (!input.contains("(define"))
      throw new RuntimeException(Messaging.printPlanningSystemMessage + "Missing (define ...) block in domain.")
  }

  // --- Main Entry Point ---
  def parseDomainStringToObject(input: String): Domain = {
    validateInput(input)
    info(Messaging.printPlanningSystemMessage + "Processing the domain specification.")
    val scanner = new lexical.Scanner(input)
    phrase(domain)(scanner) match {
      case Success(result, _) =>
        info(Messaging.printPlanningSystemMessage + "The domain specification is correctly processed.")
        performSemanticChecks(result)
        checkCustomRequirements(result)
        printDomainStatistics(result)
        runCustomHooks(result)
        result
      case Failure(msg, next) =>
        error(Messaging.printPlanningSystemMessage + s"Processing failed. Failure at [${next.pos.line}.${next.pos.column}]: $msg")
        throw new RuntimeException(s"Parse failure at [${next.pos.line}.${next.pos.column}]: $msg")
      case Error(msg, next) =>
        error(Messaging.printPlanningSystemMessage + s"Processing failed. Error at [${next.pos.line}.${next.pos.column}]: $msg")
        throw new RuntimeException(s"Parse error at [${next.pos.line}.${next.pos.column}]: $msg")
    }
  }

  def parseDomainFileToObject(filename: String): Domain =
    parseDomainStringToObject(fromFile(filename).mkString)

  def parseDomainInputStreamToObject(is: InputStream): Domain =
    parseDomainStringToObject(fromInputStream(is).mkString)

  def checkDomainStringSyntax(input: String): Unit = {
    info(Messaging.printPlanningSystemMessage + "Checking the domain specification syntax.")
    val scanner = new lexical.Scanner(input)
    phrase(domain)(scanner) match {
      case Success(_, _) =>
        info(Messaging.printPlanningSystemMessage + "The domain specification is correctly formulated.")
      case Failure(msg, next) =>
        error(Messaging.printPlanningSystemMessage + s"Syntax check failed at [${next.pos.line}.${next.pos.column}]: $msg")
        throw new RuntimeException(s"Syntax check failed at [${next.pos.line}.${next.pos.column}]: $msg")
      case Error(msg, next) =>
        error(Messaging.printPlanningSystemMessage + s"Syntax error at [${next.pos.line}.${next.pos.column}]: $msg")
        throw new RuntimeException(s"Syntax error at [${next.pos.line}.${next.pos.column}]: $msg")
    }
  }

  def checkDomainFileSyntax(filename: String): Unit =
    checkDomainStringSyntax(fromFile(filename).mkString)

  // --- Semantic Checks ---
  private def performSemanticChecks(domain: Domain): Unit = {
    checkDuplicates(domain.types, "types", (t: DomainType) => t.name)
    checkDuplicates(domain.predicates, "predicates", (p: Predicate) => p.name)
    checkDuplicates(domain.functions, "functions", (f: Function) => f.name)
    checkDuplicates(domain.operators, "actions", (o: Operator) => o._name)
    checkDuplicates(domain.tasks, "tasks", (t: Task) => t._name)
    checkTypeHierarchyCycles(domain)
    checkMethodTaskConsistency(domain)
    checkTypeReferences(domain)
    checkPredicateReferences(domain)
  }

  private def checkDuplicates[T](items: List[T], name: String, extractKey: T => String): Unit = {
    val duplicates = items.groupBy(extractKey).collect { case (k, vs) if vs.size > 1 => k }
    if (duplicates.nonEmpty) {
      val msg = Messaging.printPlanningSystemMessage +
        s"Duplicate $name detected: ${duplicates.mkString(", ")}"
      warn(msg)
      throw new RuntimeException(msg)
    }
  }

  private def checkTypeHierarchyCycles(domain: Domain): Unit = {
    val typeMap = domain.types.map(t => t.name -> t.supertype).toMap
    def hasCycle(t: String, visited: Set[String]): Boolean =
      typeMap.get(t) match {
        case Some(parent) if parent != "object" =>
          if (visited.contains(parent)) true
          else hasCycle(parent, visited + parent)
        case _ => false
      }
    domain.types.foreach { t =>
      if (hasCycle(t.name, Set(t.name)))
        throw new RuntimeException(s"Type hierarchy cycle detected at type: ${t.name}")
    }
  }

  private def checkMethodTaskConsistency(domain: Domain): Unit = {
    val taskNames = domain.tasks.map(_._name).toSet
    domain.operators.foreach { op =>
      if (taskNames.contains(op._name)) {
        info(s"Action '${op._name}' is also defined as a task.")
      }
    }
    domain.tasks.foreach { t =>
      val paramVars = t.parameters.collect { case v: Var => v }
      if (paramVars.exists(v => paramVars.count(_.name == v.name) > 1))
        warn(Messaging.printPlanningSystemMessage + s"Task '${t._name}' has duplicate parameters.")
    }
  }

  private def checkTypeReferences(domain: Domain): Unit = {
    val typeNames = domain.types.map(_.name).toSet + "object"
    domain.types.foreach { t =>
      if (!typeNames.contains(t.supertype))
        warn(Messaging.printPlanningSystemMessage + s"Type '${t.name}' has unknown supertype '${t.supertype}'.")
    }
    domain.predicates.foreach { p =>
      p.arguments.collect { case v: Var => v }.foreach { v =>
        if (!typeNames.contains(v._type))
          warn(Messaging.printPlanningSystemMessage + s"Predicate '${p.name}' has argument with unknown type '${v._type}'.")
      }
    }
  }

  private def checkPredicateReferences(domain: Domain): Unit = {
    val declaredPreds = domain.predicates.map(_.name).toSet
    domain.operators.foreach { op =>
      val usedPreds = extractPredicateNames(op.precondition)
      usedPreds.filterNot(declaredPreds.contains).foreach { missing =>
        warn(s"Predicate '$missing' used in action '${op._name}' but not declared.")
      }
    }
  }
  private def extractPredicateNames(expr: Expression): Set[String] = expr match {
    case ExpressionAtomic(name, _) => Set(name)
    case ExpressionAnd(a, b)      => extractPredicateNames(a) ++ extractPredicateNames(b)
    case ExpressionOr(a, b)       => extractPredicateNames(a) ++ extractPredicateNames(b)
    case ExpressionNot(e)         => extractPredicateNames(e)
    case _                        => Set()
  }

  // --- Custom Requirements/Features ---
  def checkCustomRequirements(domain: Domain): Unit = {
    val customReqs = domain.requirements.filter(r => r.startsWith(":my-custom-"))
    if (customReqs.nonEmpty) {
      info(s"Custom requirements found: ${customReqs.mkString(", ")}")
      // Implement your logic for custom requirements here
    }
  }

  // --- Domain Statistics ---
  def printDomainStatistics(domain: Domain): Unit = {
    println(s"Domain '${domain.name}':")
    println(s"  Requirements: ${domain.requirements.mkString(", ")}")
    println(s"  Types: ${domain.types.size}")
    println(s"  Predicates: ${domain.predicates.size}")
    println(s"  Functions: ${domain.functions.size}")
    println(s"  Actions: ${domain.operators.size}")
    println(s"  Tasks: ${domain.tasks.size}")
    println(s"  Axioms: ${domain.axioms.size}")
    // Add more as needed
  }

  // --- Visualization/Export Example ---
  def exportTypeHierarchyToDOT(domain: Domain, filename: String): Unit = {
    val lines = mutable.Buffer("digraph Types {")
    domain.types.foreach { t =>
      if (t.supertype != "object")
        lines += s"""  "${t.supertype}" -> "${t.name}";"""
    }
    lines += "}"
    import java.nio.file._
    Files.write(Paths.get(filename), lines.mkString("\n").getBytes)
  }

  // --- Add more extension methods below as needed ---
}
