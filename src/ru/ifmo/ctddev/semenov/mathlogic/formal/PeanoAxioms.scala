package ru.ifmo.ctddev.semenov.mathlogic.formal

import ru.ifmo.ctddev.semenov.mathlogic.expressions.{Matcher, Expression}
import ru.ifmo.ctddev.semenov.mathlogic.parsing.FormalParser

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object PeanoAxioms {
  private val parser = new FormalParser()

  private val axiomsString = List(
    "a=b->a'=b'",
    "a=b->a=c->b=c",
    "a'=b'->a=b",
    "!a'=0",
    "a+b'=(a+b)'",
    "a+0=a",
    "a*0=0",
    "a*b'=a*b+a"
  )

  val axioms: List[Expression] = axiomsString map parser.parse

  def getIdx(expression: Expression): Option[Int] = {
    val matches: Expression => Boolean = Matcher.exactMatch(expression)(_)
    for (idx <- axioms.indices) {
      if (matches(axioms(idx))) {
        return Some(idx)
      }
    }
    None
  }
}
