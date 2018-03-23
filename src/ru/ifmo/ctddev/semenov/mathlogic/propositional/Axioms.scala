package ru.ifmo.ctddev.semenov.mathlogic.propositional

import ru.ifmo.ctddev.semenov.mathlogic.expressions.{Expression, ReverseMatcher}
import ru.ifmo.ctddev.semenov.mathlogic.parsing.PropositionalParser

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object Axioms {
  private final val parser = new PropositionalParser(true)

  val axiomsString = List(
    "A->B->A",
    "(A->B)->(A->B->C)->(A->C)",
    "A&B->A",
    "A&B->B",
    "A->B->A&B",
    "A->A|B",
    "B->A|B",
    "(A->Q)->(B->Q)->(A|B->Q)",
    "(A->B)->(A->!B)->!A",
    "!!A->A"
  )

  val axioms = axiomsString map parser.parse

  def getIdx(expression: Expression): Option[Int] = {
    val matcher = new ReverseMatcher(expression)
    var axiomNumber = 0
    for (axiom <- axioms) {
      if (matcher(axiom)) {
        return Some(axiomNumber)
      }
      axiomNumber += 1
    }
    None
  }
}
