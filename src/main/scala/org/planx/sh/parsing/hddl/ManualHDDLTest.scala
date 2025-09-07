package org.planx.sh.parsing.hddl

object ManualHDDLAllInOneTest extends HDDLProblemParser {

  def main(args: Array[String]): Unit = {
    val domainString =
      """(define (domain basic)
        |  (:requirements :hierarchy :negative-preconditions :method-preconditions)
        |  (:predicates (have ?a))
        |
        |  (:action pickup
        |    :parameters (?a)
        |    :precondition (not (have ?a))
        |    :effect (have ?a)
        |  )
        |
        |  (:action drop
        |    :parameters (?a)
        |    :precondition (have ?a)
        |    :effect (not (have ?a))
        |  )
        |
        |  (:method have_first
        |    :parameters (?x ?y)
        |    :task (swap ?x ?y)
        |    :precondition (and
        |      (have ?x)
        |      (not (have ?y))
        |    )
        |    :ordered-subtasks (and
        |      (drop ?x)
        |      (pickup ?y)
        |    )
        |  )
        |
        |  (:method have_second
        |    :parameters (?x ?y)
        |    :task (swap ?x ?y)
        |    :precondition (and
        |      (have ?y)
        |      (not (have ?x))
        |    )
        |    :ordered-subtasks (and
        |      (drop ?y)
        |      (pickup ?x)
        |    )
        |  )
        |)
        |""".stripMargin

    val problemString =
      """(define (problem pb1)
        |  (:domain basic)
        |  (:objects kiwi banjo)
        |  (:init (have kiwi))
        |  (:htn :subtasks (swap banjo kiwi))
        |)
        |""".stripMargin

    // 1. LEXER TEST (domain)
    println("===== HDDLLexer: Tokenized Output (Domain) =====")
    val lexer = new HDDLLexer()
    var tokens = new lexer.Scanner(domainString)
    var count = 0
    val allTokens = scala.collection.mutable.Buffer.empty[String]
    while (!tokens.atEnd) {
      val token = tokens.first
      println(s"${count.formatted("%03d")} => ${token.getClass.getSimpleName}: '$token'")
      allTokens += token.toString
      tokens = tokens.rest
      count += 1
    }
    println("===== LAST 10 TOKENS (Domain) =====")
    allTokens.takeRight(10).zipWithIndex.foreach { case (tok, i) =>
      println(f"${allTokens.size - 10 + i}%03d => $tok")
    }
    println()

    // 2. HDDLDomainParser TEST (use the object, not trait)
    println("===== HDDLDomainParser: Domain Parse Output =====")
    try {
      val domain = HDDLDomainParser.parseDomainStringToObject(domainString)
      println("Domain Parse Success!")
      println(domain)
    } catch {
      case e: Exception =>
        println(s"Domain Parse Failure: ${e.getMessage}")
    }
    println()
    println("===== HDDLLexer: Tokenized Output (Problem) =====")
    val probLexer = new HDDLLexer()
    var probTokens = new probLexer.Scanner(problemString)
    var probCount = 0
    while (!probTokens.atEnd) {
      val token = probTokens.first
      println(s"${probCount.formatted("%03d")} => ${token.getClass.getSimpleName}: '$token'")
      probTokens = probTokens.rest
      probCount += 1
    }

    // 3. HDDLProblemParser TEST (problem)
    println("===== HDDLProblemParser: Problem Parse Output =====")
    val problemResult = parseProblem(problemString.trim)
    problemResult match {
      case Success(result, _) =>
        println("Problem Parse Success!")
        println(result)
      case NoSuccess(msg, next) =>
        println(s"Problem Parse Failure: $msg")
        println(s"At line ${next.pos.line}, column ${next.pos.column}")
        println(next.pos.longString)
        val errPos = next.offset
        val context = problemString.slice(math.max(0, errPos - 10), math.min(problemString.length, errPos + 10))
        println(s"Context around error: ...${context}...")
    }
    println("===== END ALL TESTS =====\n")
  }
}
