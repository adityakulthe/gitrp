package org.planx.sh.parsing.hpdl

object ManualHPDLTest extends App {
  // --- HPDL Domain String ---
  val domainString =
    """(define (domain basic)
      | (:requirements :strips :typing)
      | (:types thing)
      | (:predicates
      |  (have ?t - thing)
      | )
      |
      | (:action pickup
      |     :parameters (?pickup - thing)
      |     :precondition ()
      |     :effect (have ?pickup)
      | )
      |
      | (:action drop
      |     :parameters (?drop - thing)
      |     :precondition (have ?drop)
      |     :effect (not (have ?drop))
      | )
      |
      | (:task swap
      |     :parameters (?x - thing ?y - thing)
      |     (:method have-x
      |         :precondition (and (have ?x)(not (have ?y)))
      |         :tasks (sequence (drop ?x) (pickup ?y))
      |     )
      |     (:method have-y
      |         :precondition (and (have ?y)(not (have ?x)))
      |         :tasks (sequence (drop ?y) (pickup ?x))
      |     )
      | )
      |)
      |""".stripMargin

  // --- HPDL Problem String ---
  val problemString =
      """(define (problem exemplary-problem)
        |  (:domain basic)
        |  (:requirements :strips)
        |  (:objects kiwi banjo - thing)
        |  (:init
        |    (have kiwi)
        |  )
        |  (:goal-tasks (sequence (swap banjo kiwi)))
        |)""".stripMargin


  // 1. LEXER TEST: DOMAIN
  println("===== HPDLLexer: Tokenized Output (Domain) =====")
  val domainLexer = new HPDLLexer()
  var domainTokens = new domainLexer.Scanner(domainString)
  var count = 0
  while (!domainTokens.atEnd) {
    val token = domainTokens.first
    println(s"${count.formatted("%03d")} => ${token.getClass.getSimpleName}: '$token'")
    domainTokens = domainTokens.rest
    count += 1
  }

  // 2. LEXER TEST: PROBLEM
  println("\n===== HPDLLexer: Tokenized Output (Problem) =====")
  val problemLexer = new HPDLLexer()
  var problemTokens = new problemLexer.Scanner(problemString)
  count = 0
  while (!problemTokens.atEnd) {
    val token = problemTokens.first
    println(s"${count.formatted("%03d")} => ${token.getClass.getSimpleName}: '$token'")
    problemTokens = problemTokens.rest
    count += 1
  }

  // 3. PARSER TEST: DOMAIN
  println("\n===== HPDLDomainParser: Parse Output (Domain) =====")
  try {
    val domain = HPDLDomainParser.processDomainStringToObject(domainString)
    println("Parse Success! Parsed Domain Object:")
    println(domain)
  } catch {
    case e: Exception =>
      println("Parse Failure (Domain): " + e.getMessage)
      e.printStackTrace()
  }

  // 4. PARSER TEST: PROBLEM
  println("\n===== HPDLProblemParser: Parse Output (Problem) =====")
  try {
    val problem = HPDLProblemParser.processProblemStringToObject(problemString)
    println("Parse Success! Parsed Problem Object:")
    println(problem)
  } catch {
    case e: Exception =>
      println("Parse Failure (Problem): " + e.getMessage)
      e.printStackTrace()
  }
}
