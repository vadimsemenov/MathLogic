package ru.ifmo.ctddev.semenov.mathlogic.parsing

import ru.ifmo.ctddev.semenov.mathlogic.expressions.{Expression, Variable}

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
trait Parser {
  def parse(string: String): Expression
}

/**
  * Simple top-down Propositional Parser
  *
  * NB: not thread-safe
  */
class PropositionalParser extends Parser {
  private var lexer: Lexer = null

  override def parse(string: String) = {
    lexer = new PropositionalLexer(string)
    parseExpression()
  }

  private def parseExpression(): Expression = {
    var expression = parseDisjunction()
    if (lexer.current() == ARROW) {
      consume(ARROW)
      expression = expression -> parseExpression() // TODO: tailrec
    }
    expression
  }

  private def parseDisjunction(): Expression = {
    var disjunction = parseConjunction()
    while (lexer.current() == STICK) {
      consume(STICK)
      disjunction = disjunction | parseConjunction()
    }
    disjunction
  }

  private def parseConjunction(): Expression = {
    var conjunction = parseConjunct()
    while (lexer.current() == AMPERSAND) {
      consume(AMPERSAND)
      conjunction = conjunction & parseConjunct()
    }
    conjunction
  }

  private def parseConjunct(): Expression = lexer.current() match {
    case BANG      => consumeNegation()
    case OPENED    => consumeParenthesis()
    case VAR(name) => consumeVar(name)
    case _         => throw new IllegalStateException("not expected: " + lexer.current())
  }

  private def consumeNegation(): Expression = {
    consume(BANG)
    !parseConjunct()
  }

  private def consumeParenthesis(): Expression = {
    consume(OPENED)
    val expression = parseExpression()
    consume(CLOSED)
    expression
  }

  private def consumeVar(name: String): Expression = {
    consume(VAR(name))
    Variable(name)
  }

  private def consume(token: Lexeme): Unit = {
    if (lexer.current().getClass != token.getClass) {
      throw new IllegalStateException("expected: " + token + ", but found: " + lexer.current())
    }
    if (lexer.hasNext) {
      lexer.next()
    }
  }
}
